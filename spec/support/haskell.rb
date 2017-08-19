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

FileUtils.rm_r(root) if File.exists?(root)
Dir.mkdir(root)

dir_map.each do |from, to|
  Dir.mkdir("#{root}/#{to}")

  Dir.glob("./#{from}/**/*.hs").each do |file|
    filename = file.split("/").last
    FileUtils.cp file, "#{root}/#{to}/#{filename}"
  end
end

file_map.each do |from, to|
  FileUtils.cp from, "#{root}/#{to}"
end

exec "cd #{root}; stack test"
