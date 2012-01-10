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

task :console do
  sh "irb", "-r", File.dirname(__FILE__) + "/bee.rb", "--simple-prompt"
end
