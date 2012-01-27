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
