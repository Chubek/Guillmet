#!/usr/bin/env ruby

require 'optparse'

$PREFIX = '#'
$PARAMP = '$'
$TERMTR = ';'
$SDELIM = '"'
$INCDIR = 'include'
$DEFDIR = 'define'

class LexicalScanner
  attr_accessor :input_file, :include_dirs, :define_dirs, :bypass_lines

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

  class BypassLine
    attr_accessor :contents, :lno

    def initialize(contents, lno)
      @contents = contents
      @lno = lno
    end
  end

  def initialize(input_file)
    @input_file = input_file
    @include_dirs = []
    @define_dirs = []
    @bypass_lines = []
  end

  def scan_analyze
    raise "The file #{@input_file} does not exist" unless File.exist? @input_file
    in_file = File.open @input_file
    lno = 0

    until in_file.eof?
      ln = in_file.readline
      lno += 1

      unless ln.starts_with? $PREFIX
        @bypass_lines << BypassLine.new ln, lno
        continue
      end

      if ln.starts_with? $PREFIX + $INCDIR
        raise "Syntax error at #{lno}" unless mk_incpatt.match ln
        @include_dirs << IncludeDirective.new $2, $1 == '-s', lno
        continue
      end

      if ln.starts_with? $PREFIX + $DEFDIR
        raise "Syntax error at #{lno}" unless mk_defpatt.match ln
        @define_dirs << DefineDirective.new $1, $2
        continue
      end

      raise "Syntax error at #{lno}"
    end

    return [@bypass_lines, @define_dirs, @include_dirs]
  end

  def mk_incpatt
    return Regexp.new "^#{$PREFIX}#{$INCIDR}\s+(-s|-ns)\s+#{$SDELIM}([^#{$SDELIM}]+)#{$SDELIM}\s*#{$TERMTR}$"
  end

  def mk_defpatt
    return Regexp.new "^#{$PREFIX}#{$DEFDIR}\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+([^#{$TERMTR}]+)#{$TERMTR}\s*$"
  end
end

class SyntacticPreprocessor
  attr_reader :include_dirs, :define_dirs, :bypass_lines
  
  def initialize(include_dirs, define_dirs, bypass_lines)
    @include_dirs = include_dirs
    @define_dirs = define_dirs
    @bypass_lines = bypass_lines
  end
end

