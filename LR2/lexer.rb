KEYWORDS = [
  "PROCEDURE", "OPTIONS", "DECLARE", "VAR", "STATIC", "INITIAL", "FILE",
  "OPEN", "UPDATE", "RECORD", "TITLE", "PUT", "SKIP", "LIST", "ADDR",
  "IF", "THEN", "GOTO", "CALL", "END", "BASED",
  "DO", "WHILE", "MOD", "CONTINUE", "REPEAT", "ELSE", "LEAVE", "TO",
  "BY", "LIKE", "B", "B1", "B2", "B3", "B4"
].freeze

OPERATORS_DELIMITERS = [
  "(", ")", ";", ":", ",", "=", "+", "-", "*", "/",
  "{", "}", "[", "]", "<", ">", "."
].freeze

DATA_TYPES_LIST = [
  "CHAR", "FIXED", "DECIMAL", "BIT", "BINARY", "FLOAT", "COMPLEX",
  "LABEL", "ENTRY", "VARIABLE", "POINTER", "FILE", "CHARACTER"
].freeze

IDENTIFIERS_TABLE = {}
CONSTANTS_TABLE = {}

LEX_ERRORS = []

$token_id_counter = 1

def get_data_type(tok)
  case tok[:type]
  when "numeric_constant"
    if tok[:value].match?(/[\.eE]/)
      "Числовая константа с плавающей точкой"
    else
      "Целочисленная константа"
    end
  when "string_constant"
    if tok[:value].length == 1
      "Символьный литерал"
    else
      "Строковый литерал"
    end
  when "bit_constant"
    "Битовая константа"
  when "identifier"
    "Идентификатор"
  when "keyword"
    "Ключевое слово (KW_#{tok[:value].upcase})"
  when "operator"
    op_map = {
      "="  => "OP_ASSIGN",
      "+"  => "OP_PLUS",
      "-"  => "OP_MINUS",
      "*"  => "OP_MULT",
      "/"  => "OP_DIV",
      "("  => "OP_LPAREN",
      ")"  => "OP_RPAREN",
      ":"  => "OP_COLON",
      ";"  => "OP_SEMICOLON",
      ","  => "OP_COMMA",
      "<=" => "OP_LE",
      ">=" => "OP_GE",
      "<>" => "OP_NE",
      "!=" => "OP_NE",
      "<"  => "OP_LT",
      ">"  => "OP_GT",
      "."  => "OP_DOT"
    }
    if op_map.key?(tok[:value])
      "Оператор (#{op_map[tok[:value]]})"
    else
      "Оператор/разделитель"
    end
  else
    "Неизвестный тип"
  end
end

def tokenize(input)
  tokens = []
  parenthesis_counter = 0
  last_token_was_declare = false

  input = input.gsub(/\s+/, ' ') # Нормализация пробелов

  while input.length > 0
    input = input.lstrip # Удаление ведущих пробелов
    break if input.empty?

    # Обработка DECLARE
    if last_token_was_declare
      if input.match?(/^\d+[A-Za-z_]/)
        bad_name = input.match(/^\d+[A-Za-z0-9_]*/).to_s
        err_msg = format(
          "Лексическая ошибка (token_id %d): После DECLARE встретилась слитная лексема '%s' (уровень и идентификатор без пробела).",
          $token_id_counter, bad_name
        )
        LEX_ERRORS << err_msg
        input = input[bad_name.length..-1]
        last_token_was_declare = false
        next
      end
    end

    # Числовые константы
    if input.match?(/^\d+(\.\d+)?([eE][+-]?\d+)?/)
      val = input.match(/^\d+(\.\d+)?([eE][+-]?\d+)?/).to_s
      token = {
        tokenID: $token_id_counter,
        type: "numeric_constant",
        value: val
      }
      unless CONSTANTS_TABLE.key?(val)
        CONSTANTS_TABLE[val] = $token_id_counter
      end
      token[:pointer] = CONSTANTS_TABLE[val]
      $token_id_counter += 1
      tokens << token
      input = input[val.length..-1]
      last_token_was_declare = false
      next
    end

    # Строковые константы
    if input[0] == "'" || input[0] == '"'
      quote_char = input[0]
      remaining = input[1..-1]
      pos_quote = remaining.index(quote_char)
      pos_newline = remaining.index("\n")

      if pos_quote.nil? || (!pos_newline.nil? && pos_newline < pos_quote)
        err_msg = format(
          "Лексическая ошибка (token_id %d): Незакрытая строковая константа, начинается с %s",
          $token_id_counter, quote_char
        )
        LEX_ERRORS << err_msg
        return tokens
      else
        match_length = pos_quote + 1
        raw_val = input[0..match_length]
        if raw_val.match?(/^(['"]).*\1/)
          val = raw_val[1..-2]
          token = {
            tokenID: $token_id_counter,
            type: "string_constant",
            value: val
          }
          unless CONSTANTS_TABLE.key?(val)
            CONSTANTS_TABLE[val] = $token_id_counter
          end
          token[:pointer] = CONSTANTS_TABLE[val]
          $token_id_counter += 1
          tokens << token
          input = input[match_length + 1..-1]
          last_token_was_declare = false
          next
        else
          err_msg = format(
            "Лексическая ошибка (token_id %d): Ошибка в определении строковой константы.",
            $token_id_counter
          )
          LEX_ERRORS << err_msg
          return tokens
        end
      end
    end

    # Идентификаторы и ключевые слова
    if input.match?(/^[A-Za-z][A-Za-z0-9_]*/)
      val = input.match(/^[A-Za-z][A-Za-z0-9_]*/).to_s
      if DATA_TYPES_LIST.include?(val)
        token = {
          tokenID: $token_id_counter,
          type: "identifier",
          value: val
        }
        unless IDENTIFIERS_TABLE.key?(val)
          IDENTIFIERS_TABLE[val] = $token_id_counter
        end
        token[:pointer] = IDENTIFIERS_TABLE[val]
        last_token_was_declare = false
      elsif KEYWORDS.include?(val)
        token = {
          tokenID: $token_id_counter,
          type: "keyword",
          value: val
        }
        last_token_was_declare = (val == "DECLARE")
      else
        token = {
          tokenID: $token_id_counter,
          type: "identifier",
          value: val
        }
        unless IDENTIFIERS_TABLE.key?(val)
          IDENTIFIERS_TABLE[val] = $token_id_counter
        end
        token[:pointer] = IDENTIFIERS_TABLE[val]
        last_token_was_declare = false
      end
      $token_id_counter += 1
      tokens << token
      input = input[val.length..-1]
      next
    end

    # Многозначные операторы
    multi_operators = ["<=", ">=", "<>", "!="]
    if input.length >= 2
      potential_op = input[0..1]
      if multi_operators.include?(potential_op)
        token = {
          tokenID: $token_id_counter,
          type: "operator",
          value: potential_op
        }
        token[:pointer] = $token_id_counter
        $token_id_counter += 1
        tokens << token
        input = input[2..-1]
        last_token_was_declare = false
        next
      end
    end

    # Одиночные операторы и разделители
    op = input[0]
    if OPERATORS_DELIMITERS.include?(op)
      token = {
        tokenID: $token_id_counter,
        type: "operator",
        value: op
      }
      token[:pointer] = $token_id_counter
      $token_id_counter += 1
      tokens << token
      input = input[1..-1]
      last_token_was_declare = false
      next
    end

    # Неопознанный символ
    err_char = input[0]
    err_msg = format(
      "Лексическая ошибка (token_id %d): Неопознанный символ: '%s'",
      $token_id_counter, err_char
    )
    LEX_ERRORS << err_msg
    return tokens
  end

  tokens
end

def group_tokens(tokens)
  tokens.each do |tok|
    if tok[:type] == "identifier" && KEYWORDS.include?(tok[:value]) &&
      !DATA_TYPES_LIST.include?(tok[:value])
      tok[:type] = "keyword"
    end
  end

  groups = {
    identifiers: tokens.select { |tok| tok[:type] == "identifier" },
    constants: tokens.select { |tok| ["numeric_constant", "string_constant"].include?(tok[:type]) },
    keywords: tokens.select { |tok| tok[:type] == "keyword" },
    operators: tokens.select { |tok| tok[:type] == "operator" }
  }

  groups[:types] = groups[:identifiers].select { |tok| DATA_TYPES_LIST.include?(tok[:value]) }
  groups[:variables] = groups[:identifiers].reject { |tok| DATA_TYPES_LIST.include?(tok[:value]) }

  groups
end

def unique_tokens(token_group)
  keys = token_group.map { |tok| "#{tok[:type]}|#{tok[:value]}" }
  unique_indices = keys.each_index.select { |i| keys.index(keys[i]) == i }
  token_group.values_at(*unique_indices)
end

def print_token_group(group_name, token_group, con)
  unique_group = unique_tokens(token_group)

  id_col_width = 5
  value_col_width = 40
  type_col_width = 40

  con.puts "=== #{group_name} ==="
  header = [
    "ID".ljust(id_col_width),
    "Значение".ljust(value_col_width),
    "Тип данных".ljust(type_col_width)
  ].join
  con.puts header
  con.puts "-" * (id_col_width + value_col_width + type_col_width + 2)

  unique_group.each do |tok|
    value = tok[:value]
    dtype = get_data_type(tok)

    line = [
      tok[:tokenID].to_s.ljust(id_col_width),
      value.ljust(value_col_width),
      dtype.ljust(type_col_width)
    ].join
    con.puts line
  end
  con.puts
end

def output_results(tokens, output_filename = "LEXICAL_OUTPUT.txt")
  File.open(output_filename, "w:UTF-8") do |con|
    groups = group_tokens(tokens)

    print_token_group("Типы данных", groups[:types], con)
    print_token_group("Переменные", groups[:variables], con)
    print_token_group("Константы", groups[:constants], con)
    print_token_group("Ключевые слова", groups[:keywords], con)
    print_token_group("Операторы и разделители", groups[:operators], con)

    con.puts "=== Лексические ошибки ==="
    if LEX_ERRORS.any?
      LEX_ERRORS.each { |err| con.puts err }
    else
      con.puts "Ошибок не обнаружено."
    end
  end
end

if File.exist?("input.PL1")
  input_text = File.read("input.PL1")
  tokens = tokenize(input_text)
  output_results(tokens, "output.txt")
else
  puts "Файл не найден."
end