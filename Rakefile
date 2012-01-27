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

task :repl do
  sh "irb", "-Isrc/main/ruby/lib", "-r", File.dirname(__FILE__) + "/bin/bee.rb", "--simple-prompt"
end
