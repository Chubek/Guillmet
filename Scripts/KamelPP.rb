#!/usr/bin/env ruby

require 'optparse'

$DIR_PREFIX = '@'
$ARG_PREFIX = '#'
$EVAR_PREFIX = '$'
$TERMINTOR = ';'
$STR_DELIM = '"'
$SEPARATOR = ','
$LDELIM = '('
$RDELIM = ')'

$INCDIR_NAME = 'Incfile'
$DEFDIR_NAME = 'Defmacro'
$EXPDIR_NAME = 'Export'
$EVLDIR_NAME = 'Eval'
$EXCDIR_NAME = 'Exec'
$SYSDIR_NAME = 'System'
$TRNDIR_NAME = 'Translit'
$REGDIR_NAME = 'Regexpcc'
$IFFDIR_NAME = 'If'
$ELSDIR_NAME = 'Else'
$ELFDIR_NAME = 'Elif'
$EIFDIR_NAME = 'Endif'

class LexicalScanner
  attr_accessor :input_file, :include_dirs, :define_dirs, :bypass_lines, :slave_memory

  class IncludeDirective
    attr_accessor :path, :silent?, :lno

    def initialize(path, silent?, lno)
      @path = path
      @silent? = silent?
      @lno = lno
    end
  end

  class DefineDirective
    attr_accessor :name, :body

    def initialize(name, body)
      @name = name
      @body = body
    end
  end

  class ExportDirective
    attr_accessor :name, :val

    def initialize(name, val)
      @name = name
      @val = val
    end
  end

  class EvalDirective
    attr_accessor :value, :dest

    def initialize(value, dest)
      @value = value
      @dest = dest
    end
  end

  class ExecDirective
    attr_accessor :value, :dest

    def initialize(value, dest)
      @value = value
      @dest = dest
    end
  end

  class SystemDirective
    attr_accessor :cmd

    def initialize(cmd)
      @cmd = cmd
    end
  end

  class TranslitDirective
    attr_accessor :from_set, :to_set, :value

    def initialize(from_set, to_set, value)
      @from_set = from_set
      @to_set = to_set
      @value = value
    end
  end

  class RegexpCCDirective
    attr_accessor :patt

    def initialize(patt)
      @patt = patt
    end
  end

  class BypassLine
    attr_accessor :contents, :lno

    def initialize(contents, lno)
      @contents = contents
      @lno = lno
    end
  end

  def initialize(input_file)
    @input_file = handle_input_file input_file
    @include_dirs = {}
    @define_dirs = {}
    @bypass_lines = {}
    @slave_memory = {}
  end

  def handle_input_file(inp)
    if !inp
      return STDIN
    else
      raise "No such file #{inp}" unless File.exists? inp
      return File.open inp
    end
  end

  def scan_analyze
    lno = 0

    until in_file.eof?
      ln = in_file.readline
      lno += 1

      unless ln.starts_with? $DIR_PREFIX
        @bypass_lines[lno] = BypassLine.new ln, lno
        @slave_memory[lno] = :BYPASS
        continue
      end

      if ln.starts_with? $DIR_PREFIX + $INCDIR_NAME
        raise "Syntax error at #{lno}" unless mk_incpatt.match ln
        @include_dirs[lno] = IncludeDirective.new $2, $1 == '-s', lno
        @slave_memory[lno] = :INCDIR_NAME
        continue
      end

      if ln.starts_with? $DIR_PREFIX + $DEFDIR_NAME
        raise "Syntax error at #{lno}" unless mk_defpatt.match ln
        @define_dirs[lno] = DefineDirective.new $1, $2
        @slave_memory[lno] = :DEFDIR_NAME
        continue
      end

      raise "Syntax error at #{lno}"
    end

    return [@bypass_lines, @define_dirs, @include_dirs, @slave_memory]
  end

  def mk_incpatt
    return Regexp.new "^#{$DIR_PREFIX}#{$INCDIR_NAME}\s+(-s|-ns)\s+#{$STR_DELIM}([^#{$STR_DELIM}]+)#{$STR_DELIM}\s*#{$TERMINTOR}$"
  end

  def mk_defpatt
    return Regexp.new "^#{$DIR_PREFIX}#{$DEFDIR_NAME}\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+([^#{$TERMINTOR}]+)#{$TERMINTOR}\s*$"
  end
end

class SyntacticPreprocessor
  attr_reader :include_dirs, :define_dirs, :bypass_lines, :symtabl, :lno_cache
  
  def initialize(include_dirs, define_dirs, bypass_lines, lno_cache)
    @include_dirs = include_dirs
    @define_dirs = define_dirs
    @bypass_lines = bypass_lines
    @lno_cache = lno_cache
    @symtabl = {}
    @final_lines = []
  end

  class Macro
    attr_reader :name, :nparams, :body

    def initialize(name, body)
      @name = name
      @nparams = body.count $ARG_PREFIX
      @body = tokenize_body body
    end

    class Token
      attr_accessor :kind, :value

      def initialize(kind, value)
        @kind = kind
        @value = value
      end
    end

    class String
      def is_digit
        /[a-z]/.match? self
      end

      def is_blank
        /\s/.match? self
      end
    end

    def tokenize_body(body)
      tokens = []
      cursor = 0
      while cursor < body.length
        char = body[cursor]
        cursor += 1
        cursor += 1 while body[curosr].is_blank
        case char
        when '"'
          cursor += 1
          string_lit = '"'
          until body[cursor] == '"'
            string_lit << body[cursor]
            cursor += 1
            raise "Syntax error" if cursor >= body.length
          end
          cursor += 1
          string_lit << '"'
          tokens << Token.new :Lexeme, string_lit
        when $ARG_PREFIX
          cursor += 1
          argnum = ""
          while body[cursor].is_digit?
            argnum << body[cursor]
            cursor += 1
            raise "Syntax error" if cursor >= body.length
          end
          tokens << Token.new :Argnum, argnum.to_i
        else
          token << Token.new :Char, char
        end
      end
    end
  end

  def preprocess
    @lno_cache.each do |lno, mem|
      case mem
      when :BYPASS
        @final_lines << handle_bypass @bypass_lines[lno]
      when :INCDIR_NAME
        @final_lines << handle_include @include_dirs[lno]
      when :DEFDIR_NAME
        @final_lines << handle_define @define_dirs[lno]
      end
    end
  end

  def handle_include(dir)
    return '\n' if dir.silent? && !File.exists? dir.path
    raise "Error: Included file does not exist" if !File.exists? dir.path
    return File.read dir.path
  end

  def handle_define(dir)
    @symtabl[dir.name] = Macro.new dir.name, dir.body
  end
  
  def handle_bypass(ln)
   # TODO 
  end
end

