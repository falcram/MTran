require 'json'
require 'graphviz'
require 'fileutils'

class Parser
  def initialize(tokens)
    @tokens = tokens
    @pos = 0
    @node_id_counter = 0
    @dot_nodes = []
    @dot_edges = []
  end

  def current_token
    @pos < @tokens.size ? @tokens[@pos] : nil
  end

  def peek_token
    (@pos + 1) < @tokens.size ? @tokens[@pos + 1] : nil
  end

  def next_token
    @pos += 1
    current_token
  end

  def create_token_node(expected_type = nil)
    token = current_token
    raise "Неожиданный конец токенов" unless token

    puts "Parsed token: id=#{token[:tokenID]}, type=#{token[:type]}, value=#{token[:value]}"
    node = { type: token[:type], value: token[:value] }
    @pos += 1
    node
  end

  def parse_procedure_header
    header = { type: "ProcedureHeader" }
    header[:identifier] = create_token_node("identifier")
    header[:separator] = create_token_node("operator")
    header[:procedureKeyword] = create_token_node("keyword")
    header[:optionsKeyword] = create_token_node("keyword")
    header[:openParen] = create_token_node("operator")
    header[:options] = create_token_node("identifier")
    header[:closeParen] = create_token_node("operator")
    header[:endHeader] = create_token_node("operator")
    header
  end

  def parse_declaration
    decl = { type: "Declaration" }
    decl[:declareKeyword] = create_token_node("keyword")
    decl[:tokens] = []
    while current_token && !(current_token[:type] == "operator" && current_token[:value] == ";")
      decl[:tokens] << create_token_node
    end
    decl[:endDeclaration] = create_token_node("operator") if current_token && current_token[:value] == ";"
    decl
  end

  def parse_declarations
    decls = []
    while current_token && current_token[:type] == "keyword" && current_token[:value] == "DECLARE"
      decls << parse_declaration
    end
    decls
  end

  def parse_statement_block
    block = { type: "StatementBlock", tokens: [] }
    token = current_token
    if token && token[:type] == "keyword" && token[:value] == "END"
      block[:tokens] << create_token_node
      return block
    end

    while current_token
      token = current_token
      if token[:type] == "operator" && token[:value] == ";"
        block[:tokens] << create_token_node
        break
      end
      block[:tokens] << create_token_node
    end
    block
  end

  def parse_statements(proc_name)
    stmts = []
    while current_token
      token = current_token
      if token[:type] == "keyword" && token[:value] == "END"
        next_tok = peek_token
        if next_tok && next_tok[:type] == "identifier" && next_tok[:value].strip == proc_name
          break
        end
      end
      stmts << parse_statement_block
    end
    stmts
  end

  def parse_procedure_end(expected_name)
    proc_end = { type: "ProcedureEnd" }
    proc_end[:endKeyword] = create_token_node("keyword")
    proc_end[:identifier] = create_token_node("identifier")
    if proc_end[:identifier][:value] != expected_name
      raise "Идентификатор завершения процедуры не совпадает с именем процедуры"
    end
    proc_end[:endSymbol] = create_token_node("operator")
    proc_end
  end

  def parse_procedure
    ast = { type: "Procedure" }
    ast[:header] = parse_procedure_header
    ast[:declarations] = parse_declarations
    proc_name = ast[:header][:identifier][:value]
    ast[:statements] = parse_statements(proc_name)
    ast[:procedureEnd] = parse_procedure_end(proc_name)
    ast
  end

  def new_node_id
    @node_id_counter += 1
    "node#{@node_id_counter}"
  end

  def traverse_ast(ast, parent = nil, edge_label = nil)
    current_id = new_node_id
    label = ""

    if ast.is_a?(Hash)
      if ast.size == 2 && ast.key?(:type) && ast.key?(:value)
        label = "#{ast[:type]}: #{ast[:value]}"
      elsif ast.key?(:type)
        label = ast[:type]
      else
        label = "Object"
      end
    else
      label = ast.to_s
    end

    label = label.gsub('"', '\"')
    @dot_nodes << "  #{current_id} [label=\"#{label}\"];"

    if parent
      if edge_label
        @dot_edges << "  #{parent} -> #{current_id} [label=\"#{edge_label}\"];"
      else
        @dot_edges << "  #{parent} -> #{current_id};"
      end
    end

    if ast.is_a?(Hash) && !(ast.size == 2 && ast.key?(:type) && ast.key?(:value))
      ast.each do |name, child|
        next if name == :pointer

        if child
          if child.is_a?(Array)
            child.each { |c| traverse_ast(c, current_id, name.to_s) }
          elsif child.is_a?(Hash)
            traverse_ast(child, current_id, name.to_s)
          else
            child_id = new_node_id
            @dot_nodes << "  #{child_id} [label=\"#{child}\"];"
            @dot_edges << "  #{current_id} -> #{child_id} [label=\"#{name}\"];"
          end
        end
      end
    end
    current_id
  end

  def generate_dot(ast)
    @dot_nodes = []
    @dot_edges = []
    @node_id_counter = 0
    traverse_ast(ast)
    dot_graph = ["digraph ST {"] + @dot_nodes + @dot_edges + ["}"]
    dot_graph.join("\n")
  end

  def generate_svg(dot_string, output_file)
    g = GraphViz.parse_string(dot_string)
    g.output(svg: output_file)
  end
end

if File.exist?("tokens_output.json")
  tokens = JSON.parse(File.read("tokens_output.json"), symbolize_names: true)
  parser = Parser.new(tokens)
  ast_tree = parser.parse_procedure

  File.write("st_tree.txt", JSON.pretty_generate(ast_tree))

  dot_graph = parser.generate_dot(ast_tree)
  parser.generate_svg(dot_graph, "st_tree.svg")

  puts "ST дерево сохранено в 'st_tree.txt'"
  puts "Визуальное представление ST сохранено в 'st_tree.svg'"
end