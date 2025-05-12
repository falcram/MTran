require 'json'

# Файлы для работы
input_file = 'st_tree.txt'
output_file = 'semantic_errors.txt'
semantic_errors = []

# Ключевые слова типов
TYPE_KEYWORDS = %w[
  CHAR CHARACTER BIT FIXED DECIMAL BINARY
  FLOAT COMPLEX LABEL ENTRY FILE
  POINTER VAR STATIC BASED INITIAL LIKE
].freeze

# Известные метки и процедуры
KNOWN_LABELS_OR_PROCS = %w[test geometric_mean simple_procedure].freeze

# Чтение JSON-файла с деревом разбора
def read_json_tree(filename)
  file_content = File.read(filename)
  JSON.parse(file_content, symbolize_names: true)
rescue Errno::ENOENT
  puts "Ошибка: файл #{filename} не найден."
  exit 1
rescue JSON::ParserError
  puts "Ошибка: файл #{filename} содержит некорректный JSON."
  exit 1
end

# Извлечение типа и начального значения из токенов
def extract_type_and_initial_value(tokens)
  type_tokens = []
  init_value = nil
  in_initial = false
  buf_init = []

  tokens.each do |tok|
    tok = tok.transform_keys(&:to_sym)
    if tok[:type] == 'keyword' && tok[:value].upcase == 'INITIAL'
      in_initial = true
      next
    end

    if in_initial
      if tok[:type] == 'operator' && tok[:value] == '('
        next
      elsif tok[:type] == 'operator' && tok[:value] == ')'
        in_initial = false
        raw_init = buf_init.join
        raw_init.strip!

        if raw_init.start_with?("'", '"') && raw_init.end_with?("'", '"')
          raw_init = raw_init[1...-1]
        end

        init_value = raw_init
        buf_init = []
      else
        buf_init << tok[:value]
      end
    else
      unless %w[DECLARE VAR STATIC BASED INITIAL LABEL ENTRY POINTER FILE LIKE CHARACTER].include?(tok[:value].upcase)
        type_tokens << tok[:value]
      end
    end
  end

  raw_type = type_tokens.join(' ')
  raw_type.gsub!(/\s*\(\s*/, '(')
  raw_type.gsub!(/\s*\)\s*/, ')')
  raw_type.gsub!(/\s*,\s*/, ',')

  { type: raw_type, init: init_value }
end

# Проверка совместимости типа и начального значения
def check_type_compatibility(declared_type, init_val)
  return nil if init_val.nil?

  if declared_type.upcase.include?('CHAR')
    if init_val.match?(/^-?\d+$/) || init_val.match?(/^-?\d+\.\d+$/)
      return "Ошибка: для CHAR/CHARACTER ожидается строка, получено числовое: #{init_val}"
    end
  end

  if declared_type.upcase.include?('BIT')
    val_no_space = init_val.gsub(/\s+/, '')
    unless val_no_space.match?(/^[01]+B$/i)
      return "Ошибка: для BIT(...) нужно двоичное значение с 'B' на конце, получено: #{init_val}"
    end
  end

  if declared_type.upcase.include?('FIXED BINARY')
    unless init_val.match?(/^[0-9]+$/)
      return "Ошибка: для FIXED BINARY ожидается целое число, получено: #{init_val}"
    end
  end

  if declared_type.upcase.include?('FIXED DECIMAL')
    unless init_val.match?(/^[0-9]+(\.[0-9]+)?$/)
      return "Ошибка: для FIXED DECIMAL ожидается числовое значение, получено: #{init_val}"
    end
  end

  if declared_type.upcase.include?('FLOAT') && !declared_type.upcase.include?('COMPLEX FLOAT')
    unless init_val.match?(/^[0-9]+(\.[0-9]+)?$/)
      return "Ошибка: для FLOAT ожидается вещественное число, получено: #{init_val}"
    end
  end

  if declared_type.upcase.include?('COMPLEX FLOAT')
    unless init_val.match?(/^[0-9]+\+[0-9]+I$/i)
      return "Ошибка: для COMPLEX FLOAT ожидается a+bI, получено: #{init_val}"
    end
  end

  if declared_type.upcase.include?('DECIMAL FLOAT')
    unless init_val.match?(/^[0-9]+(\.[0-9]+)?$/)
      return "Ошибка: для DECIMAL FLOAT ожидается вещественное число, получено: #{init_val}"
    end
  end

  nil
end

# Таблица символов
symbol_table = {}

# Обработка токенов объявления
def process_declaration_tokens(tokens, symbol_table, semantic_errors)
  idx_identifier = nil
  tokens.each_with_index do |token, i|
    token = token.transform_keys(&:to_sym)
    if token[:type] == 'identifier' && !TYPE_KEYWORDS.include?(token[:value].upcase)
      idx_identifier = i
      break
    end
  end

  if idx_identifier.nil?
    semantic_errors << "Ошибка: DECLARE не содержит нормального идентификатора!"
    return
  end

  var_name = tokens[idx_identifier][:value]
  leftover = tokens[(idx_identifier + 1)..-1]

  info = extract_type_and_initial_value(leftover)
  err = check_type_compatibility(info[:type], info[:init])
  unless err.nil?
    semantic_errors << "Переменная #{var_name}: #{err}"
  end

  symbol_table[var_name] = { type: info[:type], init: info[:init] }
end

# Основной код
tree = read_json_tree(input_file)

# Обработка объявлений
if tree[:declarations]
  tree[:declarations].each do |decl|
    tokens = decl[:tokens]
    process_declaration_tokens(tokens, symbol_table, semantic_errors)
  end
end

# Обработка операторов
if tree[:statements]
  tree[:statements].each do |stmt|
    tokens = stmt[:tokens]
    next if tokens.empty?

    if tokens.first[:type] == 'keyword' && tokens.first[:value].upcase == 'DECLARE'
      process_declaration_tokens(tokens, symbol_table, semantic_errors)
    else
      i = 0
      while i < tokens.length
        token = tokens[i].transform_keys(&:to_sym)
        if token[:type] == 'identifier'
          ident = token[:value]

          if TYPE_KEYWORDS.include?(ident.upcase)
            i += 1
            next
          end
          if KNOWN_LABELS_OR_PROCS.include?(ident)
            i += 1
            next
          end

          unless symbol_table.key?(ident)
            semantic_errors << "Предупреждение: идентификатор #{ident} использован, но не объявлен (как переменная/метка/процедура)."
          end

          j = i + 1
          while j < tokens.length
            token_j = tokens[j].transform_keys(&:to_sym)
            if token_j[:type] == 'operator' && %w[. ( ) : ,].include?(token_j[:value])
              j += 1
            elsif %w[numeric_constant identifier].include?(token_j[:type])
              j += 1
            else
              break
            end
          end

          i = j
          next
        end

        i += 1
      end
    end
  end
end

# Запись результатов
if semantic_errors.empty?
  File.write(output_file, "Семантических ошибок не обнаружено.\n")
  puts "Семантических ошибок не обнаружено."
else
  File.open(output_file, 'w') do |f|
    f.puts "Найдены семантические ошибки:\n\n"
    semantic_errors.each { |err| f.puts "- #{err}" }
  end
  puts "Семантические ошибки записаны в файл '#{output_file}'."
end