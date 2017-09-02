#!/usr/bin/env ruby

# Haskell is very picky about directory structure
# copy all the file into a temporary directory then run tests

require 'fileutils'

root = ".haskell"
file_pattern = ".hs"

dir_map = {
  "lib" => "src",
  "spec" => "test",
}

file_map = {
  "data-structures.cabal" => "data-structures.cabal",
  "stack.yaml" => "stack.yaml",
  "LICENSE" => "LICENSE",
}

Dir.mkdir(root) unless File.exists?(root)

dir_map.each do |from, to|
  dir = "#{root}/#{to}"
  Dir.mkdir(dir) unless File.exists?(dir)

  Dir.glob("./#{from}/**/*.hs").each do |file|
    filename = file.split("/").last
    FileUtils.cp file, "#{root}/#{to}/#{filename}"
  end
end

file_map.each do |from, to|
  FileUtils.cp from, "#{root}/#{to}"
end


script_head = "cd #{root}"
script_body = "stack test || STATUS=$?"
script_tail = "exit $STATUS"

if ARGV.length > -0
  script_body = ARGV.map do |arg|
    pattern = arg.gsub(%r~^.*/~, '').chomp('Spec.hs')

    %Q~stack test --test-arguments '-m "#{pattern}"' || STATUS=$?~
  end.join("\n")
end

exec [script_head, script_body, script_tail].join("\n")
