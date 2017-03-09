#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'narray'
require 'gnuplot'

module ImagePlot
  class Plot
    attr_accessor :title,
      :xlabel, :ylabel, :cblabel,
      :set_params,
      :data, :maxval, :cbtype

    def initialize(d)
      @data = d
      @maxval = [d.flatten.max, d.flatten.min.abs].max
      @title = ''
      @xlabel = ''
      @ylatel = ''
      @cblabel = ''
      @cbtype = 0
      @set_params = []
    end

    def plot(outfile)

      x = [*1..(data.length)]
      y = [*1..(data[0].length)]

      Gnuplot.open do |gp|
        SPlot.new(gp) do |plot|
          case outfile
          when /^none$/
            plot.terminal 'x11'
          when /\.tex$/
            plot.terminal "tikz"
            plot.output File.expand_path(outfile.gsub(/\.tex$/, '.tex'))
          when /\.pdf$/
            plot.terminal "pdf font 'Helvetica,18'"
            plot.output File.expand_path(outfile.gsub(/\.pdf$/, '.pdf'))
          when /\.png$/
            plot.terminal "pngcairo font 'Helvetica, 22' size 960,720"
            plot.output File.expand_path(outfile.gsub(/\.png$/, '.png'))
          else
            STDERR.puts "Error: unknown filetype"
            exit
          end

          plot.set "termoption noenhanced"

          plot.title @title
          plot.xlabel @xlabel
          plot.ylabel @ylabel
          plot.cblabel @cblabel

          plot.set "view map"
          plot.set "size square"

          plot.xrange "[#{x[0] - 0.5}:#{x[-1] + 0.5}]"
          plot.yrange "[#{x[0] - 0.5}:#{y[-1] + 0.5}]"

          plot.set "key off"

          case @cbtype
          when 1
            plot.cbrange "[0.0:#{@maxval}]"
            plot.set "palette defined ( 0 '#f7f7f7', \
                                        1 '#f4a582', \
                                        1 '#f4a582', \
                                        2 '#d6604d', \
                                        2 '#d6604d', \
                                        3 '#b2182b', \
                                        3 '#b2182b', \
                                        4 '#67001f')"
          else
            plot.cbrange "[-#{@maxval}:#{@maxval}]"
            plot.set "palette defined ( 0 '#053061', \
                                        1 '#2166ac', \
                                        1 '#2166ac', \
                                        2 '#4393c3', \
                                        2 '#4393c3', \
                                        3 '#92c5de', \
                                        3 '#92c5de', \
                                        4 '#f7f7f7', \
                                        4 '#f7f7f7', \
                                        5 '#f4a582', \
                                        5 '#f4a582', \
                                        6 '#d6604d', \
                                        6 '#d6604d', \
                                        7 '#b2182b', \
                                        7 '#b2182b', \
                                        8 '#67001f')"
          end

          @set_params.each do |p|
            plot.set p
          end

          plot.data <<
          Gnuplot::DataSet.new([x, y, @data]) do |ds|
            ds.with = "image pixels"
          end
        end
      end
    end
  end
end

private

# https://github.com/rdp/ruby_gnuplot/blob/master/lib/gnuplot.rb#L358
class SPlot < Gnuplot::Plot

  def initialize (io = nil, cmd = "splot")
    @cmd = cmd
    @settings = []
    @arbitrary_lines = []
    @data = []
    @styles = []
    yield self if block_given?
    $stderr.puts "writing this to gnuplot:\n" + to_gsplot + "\n" if $VERBOSE

    if io
      io << to_gsplot
      io << store_datasets
    end
  end

  def to_gsplot (io = "")
    @settings.each do |setting|
      io << setting.map(&:to_s).join(" ") << "\n"
    end
    @styles.each{|s| io << s.to_s << "\n"}
    @arbitrary_lines.each{|line| io << line << "\n" }

    io
  end

  def store_datasets (io = "")
    if @data.size > 0
      io << @cmd << " " << @data.collect { |e| e.plot_args }.join(", ")
      io << "\n"

      v = @data.collect { |ds| ds.to_gsplot }
      io << v.compact.join("e\n")
    end
  end
end

class Array
  def to_gsplot
    f = ""

    if ( self[0].kind_of? Array ) then
      x = self[0]
      y = self[1]
      d = self[2]

      x.each_with_index do |xv, i|
        y.each_with_index do |yv, j|
          f << [ xv, yv, d[i][j] ].join(" ") << "\n"
        end
        f << "\n"
      end
    elsif ( self[0].kind_of? Numeric ) then
      self.length.times do |i| f << "#{self[i]}\n" end
    else
      self[0].zip( *self[1..-1] ).to_gsplot
    end

    f
  end
end


