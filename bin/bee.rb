#!/usr/bin/env ruby
$:.push File.dirname(__FILE__) + "/../src/main/ruby/lib"
require "term/ansicolor"
require "bee"

String.__send__(:include, Term::ANSIColor)

@vm ||= Bee::Interpreter.new
@p  ||= Bee::Parser.new

if File.exists?("runtime.bee")
  @vm.run(false, *@p.parse(File.read("runtime.bee")))
else
  $stderr.puts "cannot load runtime.bee from current directory".yellow
end

def bee(unparsed, debug = false)
  @vm.run(debug, *@p.parse(unparsed))
rescue
  @vm.input.clear
  $stderr.puts $!.to_s.red
# $stderr.puts "  " << $!.backtrace.join("\n  ")
end

def msgbanner
end

def msghelp
  puts <<-MSG
Usage: #{$0} [switches] [--] [file] [arguments]
  -h              this message
  -v              trace execution
  -e 'code'       execute code
MSG
end

def msgbanner
  puts <<-MSG
Bee: Statically-typed functional and concatenative language

Syntax
  term term                                 composition
  [term]                                    abstraction
  : name term term ... ;                    term definition
  :: name | c field field ... | ... ;       type definition

  'a                                        character
  "abc"                                     string
  12.34                                     number

Press [TAB] for tab completion
Press ^D to exit

MSG
end

# Sloppy and incorrect
trace  = ARGV.delete("-v")
help   = ARGV.delete("-h")
exec   = ARGV.index("-e")
exec &&= ARGV[exec + 1]
ARGV.delete("-e")
ARGV.delete("--")

if help
  msghelp
elsif exec
  bee(exec, trace)
elsif ARGV.empty?
  require "readline"
  msgbanner

  # Ignore ^C
  trap("INT") { system "stty", `stty -g`.chomp; puts; exit }

  # Autocomplete
  Readline.completer_word_break_characters = " \t\n"
  Readline.completion_append_character = ""
  Readline.completion_proc = lambda do |str|
    if str[0] == '"'
      Dir[str[1..-1] + "*"].map do |path|
        if File.directory?(path)
          if Dir[path + "/*"].empty?
            '"' << path << '/"'
          else
            '"' << path << "/"
          end
        else
          '"' << path << '" '
        end
      end
    else
      if str[0] == "["
        str    = str[1..-1]
        prefix = "["
      else
        prefix = ""
      end

      ( @vm.stackops \
      + @vm.primops \
      + @vm.dictionary.names ).
      grep(/^#{Regexp.escape(str)}/).map{|o| prefix + o + " " }
    end
  end

  while line = Readline.readline(">> ")
    bee(line, trace)
    $stdout.puts @vm.stack.inspect
    $stdout.puts

    unless line !~ /\S/ or Readline::HISTORY.include?(line)
      Readline::HISTORY.push(line)
    end
  end

  puts
else
  bee(File.read(ARGV[1]), trace)
end
