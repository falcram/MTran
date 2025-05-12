KEYWORDS = [
  "PROCEDURE", "OPTIONS", "DECLARE", "VAR", "STATIC", "INITIAL", "FILE",
  "OPEN", "UPDATE", "RECORD", "TITLE", "PUT", "SKIP", "LIST", "ADDR",
  "IF", "THEN", "GOTO", "CALL", "END", "BASED",
  "DO", "WHILE", "MOD", "CONTINUE", "REPEAT", "ELSE", "LEAVE", "TO",
  "BY", "LIKE"
].freeze

OPERATORS_DELIMITERS = [
  "(", ")", ";", ":", ",", "=", "+", "-", "*", "/",
  "{", "}", "[", "]", "<", ">", "."
].freeze

DATA_TYPES_LIST = [
  "CHAR", "FIXED", "DECIMAL", "BIT", "BINARY", "FLOAT", "COMPLEX",
  "LABEL", "ENTRY", "VARIABLE", "POINTER", "FILE", "CHARACTER"
].freeze

MULTI_OPERATORS = ["<=", ">=", "<>", "!="].freeze

class Lexer
  attr_reader :identifiers_table, :constants_table, :lex_errors, :tokens

  def initialize
    @identifiers_table = {}
    @constants_table = {}
    @lex_errors = []
    @tokens = []
    @token_id_counter = 1
  end

  def get_data_type(tok)
    case tok[:type]
    when "numeric_constant"
      tok[:value].match?(/[\.eE]/) ? "Числовая константа с плавающей точкой" : "Целочисленная константа"
    when "string_constant"
      tok[:value].length == 1 ? "Символьный литерал" : "Строковый литерал"
    when "bit_constant"
      "Битовая константа"
    when "identifier"
      "Идентификатор"
    when "keyword"
      "Ключевое слово (KW_#{tok[:value]})"
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
      op_map.key?(tok[:value]) ? "Оператор (#{op_map[tok[:value]]})" : "Оператор/разделитель"
    else
      "Неизвестный тип"
    end
  end

  def tokenize(input)
    original_input = input.dup
    current_index = 0
    input_length = input.length
    parenthesis_counter = 0
    last_token_was_declare = false

    skip_whitespace = lambda do
      while current_index < input_length && input[current_index] =~ /\s/
        current_index += 1
      end
    end

    while current_index < input_length
      skip_whitespace.call
      break if current_index >= input_length

      rest = input[current_index..-1]
      token = nil
      token_start = current_index

      # Check for operators first
      # Multi-character operators
      if rest.size >= 2
        potential_op = rest[0..1]
        if MULTI_OPERATORS.include?(potential_op)
          token = {
            tokenID: @token_id_counter,
            type: "operator",
            value: potential_op,
            pos: token_start + 1,
            end: token_start + 2
          }
          token[:pointer] = @token_id_counter
          @token_id_counter += 1
          @tokens << token
          current_index += 2
          next
        end
      end

      # Single-character operators
      op = rest[0]
      if OPERATORS_DELIMITERS.include?(op)
        token = {
          tokenID: @token_id_counter,
          type: "operator",
          value: op,
          pos: token_start + 1,
          end: token_start + 1
        }
        token[:pointer] = @token_id_counter
        @token_id_counter += 1
        @tokens << token
        current_index += 1
        next
      end

      # Numeric constants
      if (m = rest.match(/^[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?/))
        val = m[0]
        token = {
          tokenID: @token_id_counter,
          type: "numeric_constant",
          value: val,
          pos: token_start + 1,
          end: token_start + val.length
        }
        unless @constants_table.key?(val)
          @constants_table[val] = @token_id_counter
        end
        token[:pointer] = @constants_table[val]
        @token_id_counter += 1
        @tokens << token
        current_index += val.length
        next
      end

      # String constants
      if ["'", '"'].include?(rest[0])
        quote_char = rest[0]
        remaining = rest[1..-1]
        pos_quote = remaining.index(quote_char)
        pos_newline = remaining.index("\n")

        if pos_quote.nil? || (!pos_newline.nil? && pos_newline < pos_quote)
          err_msg = "Лексическая ошибка (token_id #{@token_id_counter}): Незакрытая строковая константа, начинается с #{quote_char}"
          @lex_errors << err_msg
          break
        else
          match_length = pos_quote + 2
          raw_val = rest[0...match_length]
          if (m_str = raw_val.match(/^(['"])(.*?)\1/))
            val = m_str[2]
            token = {
              tokenID: @token_id_counter,
              type: "string_constant",
              value: val,
              pos: token_start + 1,
              end: token_start + match_length
            }
            unless @constants_table.key?(val)
              @constants_table[val] = @token_id_counter
            end
            token[:pointer] = @constants_table[val]
            @token_id_counter += 1
            @tokens << token
            current_index += match_length
            next
          else
            err_msg = "Лексическая ошибка (token_id #{@token_id_counter}): Ошибка при определении строковой константы."
            @lex_errors << err_msg
            break
          end
        end
      end

      # Identifiers and keywords
      if (m = rest.match(/^[A-Za-z][A-Za-z0-9_]*/))
        val = m[0]
        upcase_val = val.upcase
        
        if KEYWORDS.include?(upcase_val)
          token = {
            tokenID: @token_id_counter,
            type: "keyword",
            value: upcase_val,
            pos: token_start + 1,
            end: token_start + val.length
          }
          last_token_was_declare = (upcase_val == "DECLARE")
        else
          token = {
            tokenID: @token_id_counter,
            type: "identifier",
            value: val,
            pos: token_start + 1,
            end: token_start + val.length
          }
          unless @identifiers_table.key?(val)
            @identifiers_table[val] = @token_id_counter
          end
          token[:pointer] = @identifiers_table[val]
          last_token_was_declare = false
        end
        @token_id_counter += 1
        @tokens << token
        current_index += val.length
        next
      end

      # Unknown character
      err_char = rest[0]
      err_msg = "Лексическая ошибка (token_id #{@token_id_counter}): Неопознанный символ: '#{err_char}'"
      @lex_errors << err_msg
      break
    end

    if parenthesis_counter > 0
      err_msg = "Лексическая ошибка: Незакрытых открывающих скобок: #{parenthesis_counter}"
      @lex_errors << err_msg
    end

    @tokens
  end
end

if File.exist?("input.PL1")
  input_text = File.read("input.PL1")
  lexer = Lexer.new
  token_list = lexer.tokenize(input_text)

  token_list.each do |tok|
    puts sprintf(
      "ID:%d  Тип:%s  Значение:'%s'  Позиция:%d-%d",
      tok[:tokenID], tok[:type], tok[:value], tok[:pos], tok[:end]
    )
  end

  require 'json'

  File.write("tokens_output.json", JSON.pretty_generate(token_list))
  puts "Список токенов сохранён в файл tokens_output.json"
end