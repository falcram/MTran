require 'json'

class PL1Interpreter
  def initialize
    @symbol_table = {}
    @pointer_table = {}
    @based_table = {}
    @label_indices = {}
    @debug_flag = false
  end

  def emit(msg)
    puts msg
  end

  def debug_print(*args)
    return unless @debug_flag
    debug_args = args.map do |x|
      if x.is_a?(Array) || x.is_a?(Hash)
        x.inspect
      else
        x.to_s
      end
    end
    puts "[DEBUG] #{debug_args.join(' ')}"
  end

  def determine_code_type(ast)
    return "Unknown" unless ast.key?(:declarations)

    ast[:declarations].each do |decl|
      tokens = decl[:tokens]
      next if tokens.empty?

      if tokens.first[:type] == "numeric_constant"
        return "Code 3"
      end
    end

    "Code 1"
  end

  def extract_type_and_initial_value(tokens)
    type_tokens = []
    init_value = nil
    in_initial = false
    buf_init = []

    tokens.each do |tok|
      if tok[:type] == "keyword" && tok[:value].upcase == "INITIAL"
        in_initial = true
        next
      end

      if in_initial
        if tok[:type] == "operator" && tok[:value] == "("
          next
        elsif tok[:type] == "operator" && tok[:value] == ")"
          in_initial = false
          raw_init = buf_init.join
          raw_init.strip!
          raw_init.gsub!(/^['"](.*)['"]$/, '\1')
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

  def check_type_compatibility(declared_type, init_val)
    return nil if init_val.nil?

    if declared_type.upcase.include?('CHAR')
      if init_val.match?(/^-?\d+$/) || init_val.match?(/^-?\d+\.\d+$/)
        return "Error: CHAR/CHARACTER expects string, got numeric: #{init_val}"
      end
    end

    if declared_type.upcase.include?('BIT')
      val_no_space = init_val.gsub(/\s+/, '')
      unless val_no_space.match?(/^[01]+B$/i)
        return "Error: BIT(...) requires binary value with 'B' suffix, got: #{init_val}"
      end
    end

    if declared_type.upcase.include?('FIXED BINARY')
      unless init_val.match?(/^[0-9]+$/)
        return "Error: FIXED BINARY expects integer, got: #{init_val}"
      end
    end

    if declared_type.upcase.include?('FIXED DECIMAL')
      unless init_val.match?(/^[0-9]+(\.[0-9]+)?$/)
        return "Error: FIXED DECIMAL expects numeric value, got: #{init_val}"
      end
    end

    if declared_type.upcase.include?('FLOAT') && !declared_type.upcase.include?('COMPLEX FLOAT')
      unless init_val.match?(/^[0-9]+(\.[0-9]+)?$/)
        return "Error: FLOAT expects real number, got: #{init_val}"
      end
    end

    if declared_type.upcase.include?('COMPLEX FLOAT')
      unless init_val.match?(/^[0-9]+\+[0-9]+I$/i)
        return "Error: COMPLEX FLOAT expects a+bI format, got: #{init_val}"
      end
    end

    if declared_type.upcase.include?('DECIMAL FLOAT')
      unless init_val.match?(/^[0-9]+(\.[0-9]+)?$/)
        return "Error: DECIMAL FLOAT expects real number, got: #{init_val}"
      end
    end

    nil
  end

  def process_declaration_tokens(tokens)
    idx_identifier = nil
    tokens.each_with_index do |token, i|
      if token[:type] == "identifier" && !%w[CHAR CHARACTER BIT FIXED DECIMAL BINARY FLOAT COMPLEX LABEL ENTRY FILE POINTER VAR STATIC BASED INITIAL LIKE].include?(token[:value].upcase)
        idx_identifier = i
        break
      end
    end

    if idx_identifier.nil?
      @semantic_errors << "Error: DECLARE missing valid identifier!"
      return
    end

    var_name = tokens[idx_identifier][:value]
    leftover = tokens[(idx_identifier + 1)..-1]

    info = extract_type_and_initial_value(leftover)
    err = check_type_compatibility(info[:type], info[:init])
    @semantic_errors << "Variable #{var_name}: #{err}" if err

    @symbol_table[var_name] = { type: info[:type], init: info[:init] }
  end

  def parse_numeric(x)
    debug_print("parse_numeric: x =", x)
    return nil if x.nil? || x.to_s.empty?

    x = x.to_s.strip
    return nil if x.empty?

    val = Float(x) rescue nil
    debug_print("parse_numeric: val =", val)
    val
  end

  def get_value(var_name)
    debug_print("get_value:", var_name)
    
    if @based_table.key?(var_name)
      ptr_n = @based_table[var_name]
      debug_print("get_value: found based var, pointer =", ptr_n)
      
      if @pointer_table.key?(ptr_n)
        real_var = @pointer_table[ptr_n]
        debug_print("get_value: real_var =", real_var)
        
        if @symbol_table.key?(real_var)
          value = @symbol_table[real_var]
          debug_print("get_value: value =", value)
          return value.is_a?(Hash) ? value[:init] : value
        else
          return nil
        end
      else
        return nil
      end
    elsif @symbol_table.key?(var_name)
      value = @symbol_table[var_name]
      debug_print("get_value: value =", value)
      return value.is_a?(Hash) ? value[:init] : value
    else
      return nil
    end
  end

  def set_value(var_name, new_val)
    debug_print("set_value:", var_name, "=", new_val)
    
    if @based_table.key?(var_name)
      ptr_n = @based_table[var_name]
      if @pointer_table.key?(ptr_n)
        real_var = @pointer_table[ptr_n]
        @symbol_table[real_var] = new_val
        return
      end
    end
    
    @symbol_table[var_name] = new_val
  end

  def set_pointer(ptr_name, var_name)
    debug_print("set_pointer:", ptr_name, "->", var_name)
    @pointer_table[ptr_name] = var_name
  end

  def declare_based(based_var, ptr_name)
    debug_print("declare_based:", based_var, "->", ptr_name)
    @based_table[based_var] = ptr_name
  end

  def handle_open_file(tokens)
    title_idx = tokens.index { |t| t[:value].upcase == "TITLE" }
    if title_idx.nil?
      debug_print("handle_open_file: TITLE not found")
      return
    end

    close_paren_idx = tokens.each_index.select { |i| tokens[i][:value] == ")" }
    valid_close = close_paren_idx.select { |i| i > title_idx }.min
    if valid_close.nil?
      debug_print("handle_open_file: closing parenthesis not found")
      return
    end

    file_name_token = tokens[valid_close - 1]
    file_name = file_name_token[:value].gsub(/^['"](.*)['"]$/, '\1')
    debug_print("handle_open_file: filename =", file_name)

    if File.exist?(file_name)
      debug_print("handle_open_file: file already exists")
    else
      File.write(file_name, '')
      debug_print("handle_open_file: file created")
    end
  end

  def evaluate_condition(tokens)
    debug_print("evaluate_condition: tokens =", tokens.map { |t| t[:value] })
    return false if tokens.size < 3

    left_val = parse_left_expression(tokens, 0, 0)
    op = tokens[1][:value]
    right_val = parse_compare_value(tokens[2])
    debug_print("evaluate_condition: left_val =", left_val, "op =", op, "right_val =", right_val)

    return false if left_val.nil? || right_val.nil? || left_val == :na || right_val == :na

    case op
    when "<" then left_val < right_val
    when "<=" then left_val <= right_val
    when ">" then left_val > right_val
    when ">=" then left_val >= right_val
    when "=" then left_val == right_val
    when "<>" then left_val != right_val
    else false
    end
  end

  def evaluate_expression(tokens)
    debug_print("evaluate_expression: tokens =", tokens.map { |t| t[:value] })
    return nil if tokens.empty?

    result = nil
    current_op = nil

    tokens.each do |token|
      if %w[numeric_constant string_constant].include?(token[:type])
        value = token[:value]
        if token[:type] == "numeric_constant"
          value = value.include?('.') ? value.to_f : value.to_i
        end
        result = result.nil? ? value : (current_op == "+" ? result + value : result)
      elsif token[:type] == "identifier"
        value = get_value(token[:value]) || 0
        result = result.nil? ? value : (current_op == "+" ? result + value : result)
      elsif token[:type] == "operator" && token[:value] == "+"
        current_op = token[:value]
      end
    end

    debug_print("evaluate_expression: result =", result)
    result
  end

  def parse_compare_value(token)
    debug_print("parse_compare_value: token =", token[:value], "type =", token[:type])
    case token[:type]
    when "numeric_constant"
      parse_numeric(token[:value]) || :na
    when "string_constant"
      token[:value].gsub(/^['"](.*)['"]$/, '\1')
    when "identifier"
      get_value(token[:value]) || :na
    else
      :na
    end
  end

  def parse_left_expression(tokens, start_pos, end_pos)
    debug_print("parse_left_expression: tokens =", tokens[start_pos..end_pos].map { |t| t[:value] })
    return :na if start_pos >= tokens.size || start_pos > end_pos

    if tokens[start_pos][:value].upcase == "MOD"
      debug_print("parse_left_expression: found MOD call")
      return :na if (end_pos - start_pos + 1) < 6

      arg1 = get_value(tokens[start_pos + 3][:value])
      arg2 = parse_numeric(tokens[start_pos + 5][:value])
      debug_print("MOD args:", arg1, arg2)
      return :na if arg1.nil? || arg2.nil?

      arg1.to_f % arg2.to_f
    else
      var_name = tokens[start_pos][:value]
      v = get_value(var_name)
      maybe_num = parse_numeric(v)
      debug_print("parse_left_expression: var", var_name, "=", v)
      maybe_num || v
    end
  end

  def interpret_if(tokens)
    debug_print("interpret_if: tokens =", tokens.map { |t| t[:value] })
    cond_op_pos = nil

    tokens.each_with_index do |token, i|
      if token[:type] == "operator" && %w[= < <= > >= <>].include?(token[:value])
        cond_op_pos = i
        break
      end
    end

    if cond_op_pos.nil? || cond_op_pos + 1 >= tokens.size
      return { action: "normal" }
    end

    right_val = parse_compare_value(tokens[cond_op_pos + 1])
    left_val = parse_left_expression(tokens, 1, cond_op_pos - 1)
    op = tokens[cond_op_pos][:value]

    pass = if [left_val, right_val].none? { |x| x.nil? || x == :na }
             case op
             when "=" then left_val == right_val
             when "<" then left_val < right_val
             when "<=" then left_val <= right_val
             when ">" then left_val > right_val
             when ">=" then left_val >= right_val
             when "<>" then left_val != right_val
             else false
             end
           else
             false
           end

    debug_print("interpret_if: condition =", pass)

    idx_then = tokens.index { |t| t[:value] == "THEN" }
    if idx_then.nil?
      return { action: "normal" }
    end

    idx_else = tokens.index { |t| t[:value] == "ELSE" }
    branch_tokens = if idx_else
                      pass ? tokens[(idx_then + 1)...idx_else] : tokens[(idx_else + 1)..-1]
                    else
                      pass ? tokens[(idx_then + 1)..-1] : []
                    end

    if branch_tokens.empty?
      return { action: "normal" }
    end

    first_word = branch_tokens.first[:value].upcase.strip
    if %w[LEAVE CONTINUE].include?(first_word)
      return { action: first_word.downcase }
    end

    interpret_put(branch_tokens)
    { action: "normal" }
  end

  def interpret_put(tokens)
    debug_print("interpret_put: tokens =", tokens.map { |t| t[:value] })
    open_p = tokens.index { |t| t[:value] == "(" }
    close_p = tokens.rindex { |t| t[:value] == ")" }

    if open_p && close_p && open_p < close_p
      mid = tokens[(open_p + 1)...close_p]
      out = mid.each_with_object([]) do |t, arr|
        if t[:type] == "string_constant"
          arr << t[:value].gsub(/^['"](.*)['"]$/, '\1')
        elsif t[:type] == "identifier"
          val = get_value(t[:value])
          arr << val.to_s unless val.nil?
        end
      end
      emit(out.join(" "))
    end
  end

  def interpret_assignment(tokens)
    debug_print("interpret_assignment: tokens =", tokens.map { |t| t[:value] })
    lhs = tokens[0][:value]

    if tokens.size >= 5 && tokens[3][:value] == "+"
      var_a = tokens[2][:value]
      plus_val = tokens[4][:value]
      l = parse_numeric(get_value(var_a)) || 0
      r = parse_numeric(plus_val) || 0
      set_value(lhs, l + r)
      debug_print("interpret_assignment: assigning", lhs, "=", l + r)
    elsif tokens.size >= 3 && tokens[2][:value] == "ADDR"
      if tokens.size >= 6
        set_pointer(lhs, tokens[4][:value])
        debug_print("interpret_assignment: pointer assigned", lhs, "->", tokens[4][:value])
      end
    else
      rhs = tokens[2][:value]
      maybe_n = parse_numeric(rhs)
      if maybe_n
        set_value(lhs, maybe_n)
        debug_print("interpret_assignment: assigning", lhs, "=", maybe_n)
      else
        vr = get_value(rhs) || rhs
        set_value(lhs, vr)
        debug_print("interpret_assignment: assigning", lhs, "=", vr)
      end
    end
  end

  def interpret_declare(tokens)
    debug_print("interpret_declare: tokens =", tokens.map { |t| t[:value] })
    return if tokens.size < 2

    var_n = tokens[1][:value]
    set_value(var_n, nil)

    tokens.each_cons(3) do |t1, t2, t3|
      if t1[:type] == "keyword" && t1[:value].upcase == "BASED" && t3[:type] == "identifier"
        ptr_n = t3[:value]
        declare_based(var_n, ptr_n)
        debug_print("interpret_declare: BASED", var_n, "->", ptr_n)
      end
    end
  end

  def find_matching_end(start_pos)
    (start_pos...@statements.size).each do |i|
      if @statements[i][:tokens].size == 1 && @statements[i][:tokens].first[:value].upcase == "END"
        return i
      end
    end
    -1
  end

  def parse_do_line(tokens)
    header_words = tokens.map { |t| t[:value] }
    debug_print("parse_do_line: header_words =", header_words)

    if header_words.include?(",")
      loop_var = tokens[1][:value]
      split_index = nil

      (3...tokens.size).each do |i|
        if tokens[i][:value] == loop_var && tokens[i + 1][:value] == "="
          split_index = i
          break
        end
      end

      unless split_index
        raise "No separator found between explicit list and range in LIST loop"
      end

      explicit_vals = []
      (3...split_index).each do |i|
        if tokens[i][:type] == "numeric_constant"
          explicit_vals << tokens[i][:value].to_f
        end
      end

      range_tokens = tokens[split_index..-1]
      if range_tokens.size < 5
        raise "Invalid range format in loop"
      end

      start_val = range_tokens[2][:value].to_f
      end_val = range_tokens[4][:value].to_f
      step_val = 1

      if range_tokens.map { |t| t[:value].upcase }.include?("BY")
        by_idx = range_tokens.index { |t| t[:value].upcase == "BY" }
        if range_tokens[by_idx + 1][:value] == "-"
          step_val = -range_tokens[by_idx + 2][:value].to_f
        else
          step_val = range_tokens[by_idx + 1][:value].to_f
        end
      end

      seq_vals = (start_val..end_val).step(step_val).to_a
      final_vals = explicit_vals + seq_vals
      debug_print("parse_do_line (LIST): for variable", loop_var, "values =", final_vals)
      { type: "LIST", var: loop_var, values: final_vals }
    elsif tokens[1][:value].upcase == "WHILE"
      start_paren = header_words.index("(")
      end_paren = header_words.rindex(")")
      condition_tokens = tokens[(start_paren + 1)...end_paren]
      debug_print("parse_do_line: WHILE condition tokens =", condition_tokens.map { |t| t[:value] })
      { type: "WHILE", condition: condition_tokens }
    elsif header_words.map(&:upcase).include?("REPEAT")
      eq_idx = header_words.index("=")
      repeat_idx = header_words.map(&:upcase).index("REPEAT")
      loop_var = tokens[1][:value]
      start_val = tokens[eq_idx + 1][:value].to_f
      inc_val = 1

      if repeat_idx + 1 < tokens.size && tokens[repeat_idx + 1][:value] == "("
        start_paren = repeat_idx + 1
        end_paren = find_matching_paren(tokens, start_paren)
        inc_tokens = tokens[(start_paren + 1)...end_paren]
        raw_inc = evaluate_expression(inc_tokens)
        inc_val = raw_inc - start_val unless raw_inc.nil?
        inc_val = 1 if inc_val.nil? || !inc_val.is_a?(Numeric)
      end

      if header_words.map(&:upcase).include?("WHILE")
        all_paren_idx = header_words.each_index.select { |i| header_words[i] =~ /[()]/ }
        if all_paren_idx.size >= 4
          cond_start = all_paren_idx[2]
          cond_end = all_paren_idx[3]
          condition_tokens = tokens[(cond_start + 1)...cond_end]
        else
          condition_tokens = []
        end
        { type: "REPEAT_WHILE", var: loop_var, start_val: start_val, inc: inc_val, condition: condition_tokens }
      else
        { type: "REPEAT", var: loop_var, start_val: start_val, inc: inc_val }
      end
    elsif header_words.map(&:upcase).include?("TO")
      eq_idx = header_words.index("=")
      to_idx = header_words.map(&:upcase).index("TO")
      loop_var = tokens[1][:value]
      start_val = tokens[eq_idx + 1][:value].to_f
      end_val = tokens[to_idx + 1][:value].to_f
      step_val = 1

      if header_words.map(&:upcase).include?("BY")
        by_idx = header_words.map(&:upcase).index("BY")
        if tokens[by_idx + 1][:value] == "-"
          step_val = -tokens[by_idx + 2][:value].to_f
        else
          step_val = tokens[by_idx + 1][:value].to_f
        end
      end

      if header_words.map(&:upcase).include?("WHILE")
        wh_idx = header_words.map(&:upcase).index("WHILE")
        cond_candidates = header_words.each_index.select { |i| header_words[i] == "(" && i > wh_idx }
        cond_start = cond_candidates.empty? ? nil : cond_candidates.first
        cond_candidates2 = header_words.each_index.select { |i| header_words[i] == ")" && i > wh_idx }
        cond_end = cond_candidates2.empty? ? nil : cond_candidates2.first
        condition_tokens = if cond_start && cond_end
                            tokens[(cond_start + 1)...cond_end]
                          else
                            []
                          end
        { type: "TO_WHILE", var: loop_var, start_val: start_val, end_val: end_val, step: step_val, condition: condition_tokens }
      else
        { type: "TO", var: loop_var, start_val: start_val, end_val: end_val, step: step_val }
      end
    end
  end

  def run_do_block(definition, body_start, body_end)
    debug_print("run_do_block: type =", definition[:type])
    case definition[:type]
    when "WHILE"
      n_iter = 0
      while n_iter < 1000
        n_iter += 1
        cond = evaluate_condition(definition[:condition])
        debug_print("run_do_block WHILE: condition =", cond)
        break unless cond
        res = interpret_block_slice(body_start, body_end)
        break if res[:action] == "leave"
      end
    when "REPEAT"
      set_value(definition[:var], definition[:start_val])
      n_iter = 0
      loop do
        n_iter += 1
        break if n_iter >= 1000
        res = interpret_block_slice(body_start, body_end)
        break if res[:action] == "leave"
        cur = parse_numeric(get_value(definition[:var]))
        break if cur.nil?
        set_value(definition[:var], cur + definition[:inc])
      end
    when "REPEAT_WHILE"
      set_value(definition[:var], definition[:start_val])
      n_iter = 0
      loop do
        n_iter += 1
        break if n_iter >= 1000
        cond = evaluate_condition(definition[:condition])
        break unless cond
        res = interpret_block_slice(body_start, body_end)
        break if res[:action] == "leave"
        cur = parse_numeric(get_value(definition[:var]))
        break if cur.nil?
        set_value(definition[:var], cur + definition[:inc])
      end
    when "TO"
      set_value(definition[:var], definition[:start_val])
      n_iter = 0
      loop do
        n_iter += 1
        cur = parse_numeric(get_value(definition[:var]))
        break if cur.nil?
        if definition[:start_val] <= definition[:end_val]
          break if cur > definition[:end_val]
        else
          break if cur < definition[:end_val]
        end
        res = interpret_block_slice(body_start, body_end)
        break if res[:action] == "leave"
        set_value(definition[:var], cur + definition[:step])
      end
    when "TO_WHILE"
      set_value(definition[:var], definition[:start_val])
      n_iter = 0
      loop do
        n_iter += 1
        cur = parse_numeric(get_value(definition[:var]))
        break if cur.nil?
        if definition[:start_val] <= definition[:end_val]
          break if cur > definition[:end_val]
        else
          break if cur < definition[:end_val]
        end
        cond = evaluate_condition(definition[:condition])
        break unless cond
        res = interpret_block_slice(body_start, body_end)
        break if res[:action] == "leave"
        set_value(definition[:var], cur + definition[:step])
      end
    when "LIST"
      definition[:values].each do |val|
        set_value(definition[:var], val)
        res = interpret_block_slice(body_start, body_end)
        break if res[:action] == "leave"
      end
    end
    "normal"
  end

  def interpret_block_slice(from, to)
    pc = from
    while pc <= to
      if pc < to &&
         @statements[pc][:tokens].first[:value].upcase == "IF" &&
         @statements[pc + 1][:tokens].first[:value].upcase == "ELSE"
        combined_tokens = @statements[pc][:tokens] + @statements[pc + 1][:tokens]
        debug_print("interpret_block_slice: combining IF and ELSE:", combined_tokens.map { |t| t[:value] })
        r = interpret_if(combined_tokens)
        return r if %w[jump continue leave].include?(r[:action])
        pc += 2
        next
      end

      toks = @statements[pc][:tokens]
      debug_print("interpret_block_slice: pc =", pc, "toks =", toks.map { |t| t[:value] })

      if toks.size == 1 && toks.first[:value].upcase == "END"
        return { action: "normal", next_pc: pc + 1 }
      end

      if toks.size >= 2 && toks[1][:value] == ":"
        new_tokens = toks[2..-1]
        unless new_tokens.empty?
          r = interpret_one_statement_custom(new_tokens)
          if r[:action] == "jump"
            pc = r[:next_pc]
            next
          end
        end
        pc += 1
        next
      end

      r = interpret_one_statement(pc)
      return r if %w[jump continue leave].include?(r[:action])
      pc = r[:next_pc]
    end
    { action: "normal", next_pc: pc }
  end

  def interpret_one_statement(pc)
    tokens = @statements[pc][:tokens]
    debug_print("interpret_one_statement: pc =", pc, "tokens =", tokens.map { |t| t[:value] })

    return { action: "normal", next_pc: pc + 1 } if tokens.empty?

    cmd = tokens.first[:value].upcase

    case cmd
    when "END"
      { action: "normal", next_pc: pc + 1 }
    when "OPEN"
      handle_open_file(tokens)
      { action: "normal", next_pc: pc + 1 }
    when "IF"
      r = interpret_if(tokens)
      { action: r[:action], next_pc: pc + 1 }
    when "PUT"
      interpret_put(tokens)
      { action: "normal", next_pc: pc + 1 }
    when "GOTO"
      lab_var = tokens[1][:value]
      lab_val = get_value(lab_var)
      debug_print("GOTO: lab_var =", lab_var, "lab_val =", lab_val)
      if lab_val && lab_val != :na
        jump_idx = @label_indices[lab_val.to_s]
        return { action: "jump", next_pc: jump_idx } if jump_idx
      end
      { action: "normal", next_pc: pc + 1 }
    when "CALL"
      proc_name = tokens[1][:value]
      pval = get_value(proc_name)
      if pval && pval != :na
        j = @label_indices[pval.to_s]
        return { action: "jump", next_pc: j } if j
      end
      { action: "normal", next_pc: pc + 1 }
    when "DECLARE"
      interpret_declare(tokens)
      { action: "normal", next_pc: pc + 1 }
    when "CONTINUE", "LEAVE"
      { action: cmd.downcase, next_pc: pc + 1 }
    else
      if tokens.first[:type] == "identifier" &&
         tokens.any? { |t| t[:value] =~ /[().]/ } &&
         tokens.any? { |t| t[:value] == "=" }
        interpret_assignment(tokens)
      elsif cmd == "DO"
        end_pos = find_matching_end(pc + 1)
        debug_print("DO: found END at position", end_pos)
        if end_pos >= 0
          definition = parse_do_line(tokens)
          debug_print("DO: definition =", definition)
          run_do_block(definition, pc + 1, end_pos - 1)
          return { action: "normal", next_pc: end_pos + 1 }
        end
      end
      { action: "normal", next_pc: pc + 1 }
    end
  end

  def interpret_one_statement_custom(tokens)
    return { action: "normal", next_pc: nil } if tokens.empty?

    cmd = tokens.first[:value].upcase

    case cmd
    when "END"
      { action: "normal", next_pc: nil }
    when "ELSE"
      interpret_one_statement_custom(tokens[1..-1])
    when "IF"
      r = interpret_if(tokens)
      { action: r[:action], next_pc: nil }
    when "PUT"
      interpret_put(tokens)
      { action: "normal", next_pc: nil }
    when "GOTO"
      lab_var = tokens[1][:value]
      lab_val = get_value(lab_var)
      if lab_val && lab_val != :na
        jump_idx = @label_indices[lab_val.to_s]
        return { action: "jump", next_pc: jump_idx } if jump_idx
      end
      { action: "normal", next_pc: nil }
    when "CALL"
      proc_name = tokens[1][:value]
      pval = get_value(proc_name)
      if pval && pval != :na
        j = @label_indices[pval.to_s]
        return { action: "jump", next_pc: j } if j
      end
      { action: "normal", next_pc: nil }
    when "DECLARE"
      interpret_declare(tokens)
      { action: "normal", next_pc: nil }
    when "CONTINUE", "LEAVE"
      { action: cmd.downcase, next_pc: nil }
    else
      if tokens.first[:type] == "identifier" &&
         tokens.size >= 3 && tokens[1][:value] == "="
        interpret_assignment(tokens)
      end
      { action: "normal", next_pc: nil }
    end
  end

  def interpret_block(start_pc = 0)
    pc = start_pc
    while pc < @statements.size
      tokens = @statements[pc][:tokens]
      debug_print("interpret_block: pc =", pc, "tokens =", tokens.map { |t| t[:value] })

      if tokens.empty?
        pc += 1
        next
      end

      if tokens.size == 1 && tokens.first[:value].upcase == "END"
        return { action: "normal", pc: pc + 1 }
      end

      if tokens.size >= 2 && tokens[1][:value] == ":"
        new_tokens = tokens[2..-1]
        unless new_tokens.empty?
          r = interpret_one_statement_custom(new_tokens)
          if r[:action] == "jump"
            pc = r[:next_pc]
            next
          end
        end
        pc += 1
        next
      end

      r = interpret_one_statement(pc)
      if r[:action] == "jump"
        pc = r[:next_pc]
        next
      end
      if %w[continue leave].include?(r[:action])
        return r
      end
      pc = r[:next_pc]
    end
    { action: "normal", pc: pc }
  end

  def find_matching_paren(tokens, start_index)
    count = 0
    (start_index...tokens.size).each do |i|
      if tokens[i][:value] == "("
        count += 1
      elsif tokens[i][:value] == ")"
        count -= 1
        return i if count == 0
      end
    end
    nil
  end

  def run(ast)
    @semantic_errors = []
    @statements = ast[:statements] || []
    
    if ast[:declarations]
      ast[:declarations].each do |decl|
        process_declaration_tokens(decl[:tokens])
      end
    end

    @label_indices = {}
    @statements.each_with_index do |stmt, i|
      tokens = stmt[:tokens]
      if tokens.size >= 2 && tokens[1][:value] == ":"
        @label_indices[tokens[0][:value]] = i
      end
    end

    interpret_block

    emit("=== Interpretation completed ===")
    
    if @semantic_errors.any?
      emit("Semantic errors found:")
      @semantic_errors.each { |err| emit("- #{err}") }
      return "Code 3"
    else
      return "Code 1"
    end
  end
end


if __FILE__ == $0
  ast = JSON.parse(File.read('st_tree.txt'), symbolize_names: true)
  
  interpreter = PL1Interpreter.new
  result = interpreter.run(ast)
  puts "Program finished with: #{result}"
end