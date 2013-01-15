#!/usr/bin/env ruby

class ParsableLine < String
  def intialize(str)
    replace(str.chop!)
  end

  def group?()
    return true if self =~ /^\[.*\]$/
    return false
  end

  def key?()
    return true if self =~ /(.*?)=(.*?)/
    return false
  end
end

class IniFile
  def initialize(file)
    @groups = Hash.new()
    group   = String.new()
    keys    = Hash.new()

    File.open(file).readlines.each do | line |
      line = ParsableLine.new(line)
      next if line.empty?
      if line.group?
        keys = Hash.new()
        group = line.gsub(/(^\[)(.*?[^\[])(\]$)/, '\2').chomp
        next
      elsif line.key?
        key, value = line.split(/\s*=\s*/)
        key.gsub!(/\s/, '')
        value.gsub!(/^\s|\s$|[#;].*$/, '') if value
        keys.store(key, value)
      end
      if group.length > 0 and not key.nil? and key.size > 0
        @groups.store(group, keys)
      end
    end
  end

  def key?(group, key)
    if group?(group)
      keys = @groups[group]
      return keys.has_key?(key)
    end
  end

  def group?(group)
    return @groups.has_key?(group)
  end

  def readEntry(group, key)
    return if not group?(group)
    keys = @groups[group]
    return keys[key] if keys.has_key?(key)
    return nil
  end
end

class Metadata < IniFile
  def initialzie(file)
    super(file)
  end

  def relative_path?()
    # TODO: impl
  end

  def name?()
    return readEntry("Desktop Entry", "X-KDE-PluginInfo-Name")
  end

  def author?()
    return readEntry("Desktop Entry", "X-KDE-PluginInfo-Author")
  end

  def email?()
    return readEntry("Desktop Entry", "X-KDE-PluginInfo-Email")
  end

  def license?()
    return readEntry("Desktop Entry", "X-KDE-PluginInfo-License")
  end
end

Dir.glob("*/metadata.desktop").each do | metadataPath |
  metadata = Metadata.new(metadataPath)
  puts "Files: #{metadataPath.split("/")[0]}/*"
  puts "Copyright: #{metadata.author?} <#{metadata.email?}>"
  license = metadata.license?
  if license == "LGPLv3"
    license = "LGPL-3"
  else
    license = "UNKNOWN"
  end
  puts "License: #{license}"
  puts ""
end
