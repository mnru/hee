namespace :sbt do
  def sbt(*args)
    sh File.dirname(__FILE__) + "/bin/sbt.sh", *args
  end

  task :console do
    sbt "console"
  end

  task :compile do
    sbt "compile"
  end

  task :test do
    sbt "test"
  end
end

namespace :hee do
  task :clean do
    system "find", "src/main/haskell",
      "(",  "-name", "*.hi",
      "-o", "-name", "*.o", ")",
      "-print", "-delete"

    rm_f "src/main/haskell/Hee/Test"
  end

  task :check do
    chdir "src/main/haskell"
    extra = (RUBY_PLATFORM =~ /darwin/) ?
      ['-optl"-Wl,-read_only_relocs,suppress"'] : []

    system "ghc", "-O2", "--make", "Hee/Test", *extra
      exec "./Hee/Test", *ARGV[1..-1]
  end
end

task :repl do
  sh "./bin/bee.rb", *ARGV[1..-1]
# sh "irb", "-Isrc/main/ruby/lib", "-r", File.dirname(__FILE__) + "/bin/bee.rb", "--simple-prompt"
end
