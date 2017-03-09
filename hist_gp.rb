# -*- coding: utf-8 -*-

# hist_gp.rb
#
# Author: Rintaro Okamura
#
# Description:
#   Histogram libraries for ruby using gnuplot gem
#

require 'gnuplot'

module HistGnuplot
  class Mean
    attr_accessor :xdata, :ydata,
      :title, :xlabel, :ylabel,
      :binres,
      :interval,
      :dx, :x,
      :max_x, :max_y,
      :min_x, :min_y,
      :countlimit,
      :withstddev, :titles,
      :keyplace,
      :set_params,
      :xlogscale

    def initialize(xdata, ydata, bin=100)
      @binres = bin

      @xdata = xdata
      @ydata = ydata

      @max_x = xdata.map(&:to_f).reject(&:nan?).max.to_f
      @min_x = xdata.map(&:to_f).reject(&:nan?).min.to_f
      @max_y = ydata.map {|xs| xs.map(&:to_f).reject(&:nan?).max }.max
      @min_y = ydata.map {|xs| xs.map(&:to_f).reject(&:nan?).min }.min

      @max_x = 999.0 if @max_x.infinite?
      @max_y = 999.0 if @max_y.infinite?
      @min_x = -999.0 if @min_x.infinite?
      @min_y = -999.0 if @min_y.infinite?

      @withstddev = false
      @keyplace = 'left top'
      @countlimit = -1

      @set_params = []

      @xlogscale = false
    end

    def plot(outfilepath='none')

      means = Array.new(@xdata.length).map{Array.new(@binres)}
      stddevs1 = Array.new(@xdata.length).map{Array.new(@binres)}
      stddevs2 = Array.new(@xdata.length).map{Array.new(@binres)}

      if @xlogscale
        @interval = ((Math::log10(@max_x) - Math::log10([@min_x, 0.000001].max)).to_f / @binres).to_f

        @dx = @interval / 2.0

        @x = []

        @binres.times do |i|
          @x.push(Math::log10([@min_x, 0.000001].max) + 10.0 ** (@interval * (i + 1).to_f - @dx))
        end

        del_ind = []
        @binres.times do |ix|
          insidebin_x_index = @xdata.each_with_index.select do |a|
            a[0] >= (@x[ix]) and a[0] < (@x[ix] + 10.0 ** @interval)
          end.map{|a| a[1]}
          insidebin_y = Array.new(@ydata.length).map{[]}
          @ydata.each_with_index do |ys, i|
            insidebin_x_index.each do |j|
              insidebin_y[i].push(ys[j])
            end
          end
          if insidebin_y.map(&:length).count{ |x| x >= @countlimit} == @ydata.length
            @ydata.each_with_index do |ys, i|
              means[i][ix] = insidebin_y[i].mean
              stddevs1[i][ix] = insidebin_y[i].mean + insidebin_y[i].stddev
              stddevs2[i][ix] = insidebin_y[i].mean - insidebin_y[i].stddev
            end
          else
            del_ind.push(ix)
          end
        end

      else
        @interval = ((@max_x - @min_x).to_f / @binres).to_f

        @dx = @interval / 2.0

        @x = []

        @binres.times do |i|
          @x.push(@min_x + @interval * (i + 1).to_f - @dx)
        end

        del_ind = []
        @binres.times do |ix|
          insidebin_x_index = @xdata.each_with_index.select do |a|
            a[0] >= (@x[ix]) and a[0] < (@x[ix] + @interval)
          end.map{|a| a[1]}
          insidebin_y = Array.new(@ydata.length).map{[]}
          @ydata.each_with_index do |ys, i|
            insidebin_x_index.each do |j|
              insidebin_y[i].push(ys[j])
            end
          end
          if insidebin_y.map(&:length).count{ |x| x >= @countlimit} == @ydata.length
            @ydata.each_with_index do |ys, i|
              means[i][ix] = insidebin_y[i].mean
              stddevs1[i][ix] = insidebin_y[i].mean + insidebin_y[i].stddev
              stddevs2[i][ix] = insidebin_y[i].mean - insidebin_y[i].stddev
            end
          else
            del_ind.push(ix)
          end
        end
      end

      del_ind.sort.reverse.each do |ix|
        @ydata.each_with_index do |ys, i|
          means[i].delete_at(ix)
          stddevs1[i].delete_at(ix)
          stddevs2[i].delete_at(ix)
        end
        @x.delete_at(ix)
      end

      Gnuplot.open do |gp|
        Gnuplot::Plot.new(gp) do |plot|
          case outfilepath
          when /^none$/
            plot.terminal 'x11'
          when /\.tex$/
            plot.terminal "tikz"
            plot.output File.expand_path(outfilepath)
          when /\.pdf$/
            plot.terminal "pdf font 'Helvetica, 22'"
            plot.output File.expand_path(outfilepath)
          when /\.png$/
            plot.terminal "pngcairo font 'Helvetica, 22' size 960,720"
            plot.output File.expand_path(outfilepath)
          else
            STDERR.puts "Error: unknown filetype"
            exit
          end

          plot.title @title

          plot.xlabel @xlabel
          plot.ylabel @ylabel

          plot.set "datafile missing \"Infinity\""

          plot.set "key #{@keyplace}"

          plot.set "view map"
          plot.set "size square"

          plot.set "grid"

          if @xlogscale
            plot.set "logscale x"
            plot.xrange "[#{[@min_x,0.000001].max}:#{@max_x}]"
          else
            plot.xrange "[#{@min_x}:#{@max_x}]"
          end
          plot.yrange "[#{@min_y}:#{@max_y}]"

          @set_params.each do |p|
            plot.set p
          end

          @ydata.each_with_index do |ys, i|
            plot.data <<
            Gnuplot::DataSet.new([@x, means[i]]) do |ds|
              ds.with = "lines lc #{i + 1} lw 2"
              if @titles
                ds.title = "#{@titles[i]}"
              end
            end

            if @withstddev
              plot.data <<
              Gnuplot::DataSet.new([@x, stddevs1[i]]) do |ds|
                ds.with = "lines dashtype 2 lc #{i + 1} lw 1.2"
                ds.title = ''
              end
              plot.data <<
              Gnuplot::DataSet.new([@x, stddevs2[i]]) do |ds|
                ds.with = "lines dashtype 2 lc #{i + 1} lw 1.2"
                ds.title = ''
              end
            end
          end
        end
      end
    end
  end

  # 2D histogram
  class Hist2D
    attr_accessor :xdata, :ydata,
      :title, :xlabel, :ylabel, :cblabel,
      :binres_x, :binres_y,
      :interval_x, :interval_y,
      :dx, :dy, :x, :y, 
      :max_x, :max_y,
      :min_x, :min_y,
      :logscale, :xlogscale,
      :set_params,
      :withmedian, :withquartile, :withone_one

    def initialize(xdata, ydata, bin_x=100, bin_y=100)
      @binres_x = bin_x
      @binres_y = bin_y

      @xdata = xdata
      @ydata = ydata

      @max_x = xdata.map(&:to_f).reject(&:nan?).max.to_f
      @min_x = xdata.map(&:to_f).reject(&:nan?).min.to_f
      @max_y = ydata.map(&:to_f).reject(&:nan?).max.to_f
      @min_y = ydata.map(&:to_f).reject(&:nan?).min.to_f

      @max_x = 999.0 if @max_x.infinite?
      @max_y = 999.0 if @max_y.infinite?
      @min_x = -999.0 if @min_x.infinite?
      @min_y = -999.0 if @min_y.infinite?

      @logscale = false
      @xlogscale = false

      @set_params = []

      @withmedian = false
      @withquartile = false
      @withone_one = false
    end

    def plot(outfilepath='none')
      if @xlogscale
        @interval_x = ((Math::log10(@max_x) - Math::log10([@min_x, 0.000001].max)).to_f / @binres_x).to_f

        @dx = @interval_x / 2.0

        @x = []

        @binres_x.times do |i|
          @x.push(Math::log10([@min_x, 0.000001].max) + @interval_x * (i + 1).to_f - @dx)
        end
      else
        @interval_x = ((@max_x - @min_x).to_f / @binres_x).to_f

        @dx = @interval_x / 2.0

        @x = []

        @binres_x.times do |i|
          @x.push(@min_x + @interval_x * (i + 1).to_f - @dx)
        end
      end
      @interval_y = ((@max_y - @min_y).to_f / @binres_y).to_f

      @dy = @interval_y / 2.0

      @y = []

      @binres_y.times do |i|
        @y.push(@min_y + @interval_y * (i + 1).to_f - @dy)
      end

      if @xlogscale
        hist = Array.new(@binres_x).map{Array.new(@binres_y, 0)}
        @binres_x.times do |ix|
          insidebin_x_index = @xdata.each_with_index.select {|a|
            Math::log10(a[0]) >= (@x[ix]) and Math::log10(a[0]) < (@x[ix] + @interval_x)
          }.map{|a| a[1]}
          @binres_y.times do |iy|
            insidebin_y_index = @ydata.each_with_index.select {|a|
              a[0] >= (@y[iy]) and a[0] < (@y[iy] + @interval_y)
            }.map{|a| a[1]}
            hist[ix][iy] = (insidebin_x_index & insidebin_y_index).length
          end
        end
      else
        hist = Array.new(@binres_x).map{Array.new(@binres_y, 0)}
        @binres_x.times do |ix|
          insidebin_x_index = @xdata.each_with_index.select {|a|
            a[0] >= (@x[ix]) and a[0] < (@x[ix] + @interval_x)
          }.map{|a| a[1]}
          @binres_y.times do |iy|
            insidebin_y_index = @ydata.each_with_index.select {|a|
              a[0] >= (@y[iy]) and a[0] < (@y[iy] + @interval_y)
            }.map{|a| a[1]}
            hist[ix][iy] = (insidebin_x_index & insidebin_y_index).length
          end
        end
      end

      Gnuplot.open do |gp|
        SPlot.new(gp) do |plot|
          case outfilepath
          when /^none$/
            plot.terminal 'x11'
          when /\.tex$/
            plot.terminal "tikz"
            plot.output File.expand_path(outfilepath)
          when /\.pdf$/
            plot.terminal "pdf font 'Helvetica, 18'"
            plot.output File.expand_path(outfilepath)
          when /\.png$/
            plot.terminal "pngcairo font 'Helvetica, 22' size 960,720"
            plot.output File.expand_path(outfilepath)
          else
            STDERR.puts "Error: unknown filetype"
            exit
          end

          plot.title @title

          plot.xlabel @xlabel
          plot.ylabel @ylabel
          plot.cblabel @cblabel

          plot.set "datafile missing \"Infinity\""

          plot.set "key off"

          plot.set "view map"
          plot.set "size square"


          if @xlogscale
            plot.xrange "[#{Math::log10([@min_x,0.000001].max)}:#{Math::log10(@max_x)}]"
            plot.set "xtics (\"0.001\" #{Math::log10(0.001)}, \"0.01\" #{Math::log10(0.01)}, \"0.1\" #{Math::log10(0.1)}, \"1\" #{Math::log10(1.0)}, \"10\" #{Math::log10(10.0)}, \"100\" #{Math::log10(100.0)})"
          else
            plot.xrange "[#{@min_x}:#{@max_x}]"
          end
          plot.yrange "[#{@min_y}:#{@max_y}]"

          if @logscale 
            plot.cbrange "[1:#{hist.flatten.max}]"
            plot.set "logscale cb"
          else
            plot.cbrange "[0:#{hist.flatten.max}]"
          end

          plot.set "palette defined ( 0 '#000090', 1 '#000fff', 2 '#0090ff', 3 '#0fffee', 4 '#90ff70', 5 '#ffee00', 6 '#ff7000', 7 '#ee0000', 8 '#7f0000')"

          @set_params.each do |p|
            plot.set p
          end

          plot.data <<
          Gnuplot::DataSet.new([@x, @y, hist]) do |ds|
            ds.with = 'image pixels'
          end
        end
        if @withmedian
          Gnuplot::Plot.new(gp) do |plot|
            case outfilepath
            when /^none$/
              plot.terminal 'x11'
            when /\.tex$/
              plot.terminal "tikz"
              plot.output File.expand_path(outfilepath.gsub(/\.tex$/, '_median.tex'))
            when /\.pdf$/
              plot.terminal "pdf"
              plot.output File.expand_path(outfilepath.gsub(/\.pdf$/, '_median.pdf'))
            when /\.png$/
              plot.terminal "pngcairo font 'Helvetica, 22' size 960,720"
              plot.output File.expand_path(outfilepath.gsub(/\.png$/, '_median.png'))
            else
              STDERR.puts "Error: unknown filetype"
              exit
            end

            plot.title @title

            plot.xlabel @xlabel
            plot.ylabel @ylabel
            plot.cblabel @cblabel

            plot.set "datafile missing \"Infinity\""

            plot.set "key off"

            plot.set "view map"
            plot.set "size square"

            plot.xrange "[#{@min_x}:#{@max_x}]"
            plot.yrange "[#{@min_y}:#{@max_y}]"

            plot.data <<
            Gnuplot::DataSet.new([@x, hist.map{|ys| @y[calc_percentile_index(ys, 0.5)]}]) do |ds|
              ds.with = 'lines lc black'
            end

            if @withquartile
              plot.data <<
              Gnuplot::DataSet.new([@x, hist.map{|ys| @y[calc_percentile_index(ys, 0.25)]}]) do |ds|
                ds.with = "lines dashtype 2 lc black"
              end
              plot.data <<
              Gnuplot::DataSet.new([@x, hist.map{|ys| @y[calc_percentile_index(ys, 0.75)]}]) do |ds|
                ds.with = "lines dashtype 2 lc black"
              end
            end
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

  def sum
    reduce(:+)
  end

  def mean
    sum.to_f / size
  end

  def var
    m = mean
    reduce(0) do |a,b|
      a + (b - m) ** 2
    end / (size - 1)
  end

  def stddev
    Math.sqrt(var)
  end
end

def calc_percentile_index(ys, p)
  sum = ys.inject(:+)
  iter = 0
  ys.each_with_index do |y, i|
    return i if sum * p <= iter
    iter += y
  end
end
