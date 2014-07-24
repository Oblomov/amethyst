#!/usr/bin/ruby

=begin
AMETHYST is a Ruby library/script to do some basic statistical analysis
on series of numbers.

As a Ruby library, it provides an Amethyst::DataSet class which exposes
a number of methods to corresponding to basic statistical properties of
a dataset.

As a script, it can be used as a filter from the command line to extract
essential statistics about series of values. In script mode, it can also
provide gnuplot commands to display a nice plot about the mentioned values.
These can be forced with the --histogram and --boxplot command-line parameters,
and are automatically enabled if the output is redirected (disable with --no-histogram
--no-boxplot).

Copyright (C) 2014 Giuseppe Bilotta

Licensed under the GNU Lesser General Public License, version 2.1
See COPYING for details.
=end

=begin
TODO:

* provide an --inline option to read multiple values per line (assume no comments)
* provide a --round option to round values to a given number of digits
* provide a -p option to automatically call gnuplot
* provide a --term option to set the gnuplot terminal type and options
* --dumb and --term should (mutually exclusive) should imply -p
* improve from, to and step for outliers in boxplot

=end

module Amethyst
	VERSION=1.0

	# find the median of an array of data
	def self.median(ar)
		ds = ar.size
		if ds.odd?
			return ar[(ds-1)/2]
		else
			med_f = (ar[ds/2] + ar[ds/2-1])/2.0
			med_i = med_f.to_i
			return med_f == med_i ? med_i : med_f
		end
	end

	class DataSet
		attr_reader :size
		attr_reader :data
		attr_reader :comments

		def add_datum(val, comment=nil)
			vi = val.to_i
			vf = val.to_f
			@data << (vi == vf ? vi : vf)
			@comments << (comment ? comment : val)
		end

		# Data in a DataSet is composed of values and comments
		# It can be initialized either from a an array of values (no comments in each)
		# or from an array of pairs (copied to the internal data) or from another dataset
		def initialize(from=nil)
			case from
			when DataSet
				@data = from.data.clone
				@comments = from.comments.clone
			when Array
				@data = []
				@comments = []
				from.each do |v|
					if v.respond_to? :first and v.respond_to? :last
						self.add_datum v.first, v.last
					else
						self.add_datum v
					end
				end
				# sort the data, makes finding quantiles easier
				@data, @comments = [@data, @comments].transpose.sort.transpose
			else
				raise TypeError, "cannot generate a #{self.class} from a #{from.class}"
			end

			@size = @data.size

			@min = @max = nil
			@mid = nil
			@mean = nil
			@variance = nil
			@stddev = nil
			@median = nil
			@quartile = []
			@iqr = nil
			@mode = []
			@binwidth = nil
			@histogram = []
		end

		# TODO <<

		def min
			@min, @max = @data.minmax if @min.nil?
			return @min
		end

		def max
			@min, @max = @data.minmax if @max.nil?
			return @max
		end

		def mid
			if @mid.nil?
				mid_f = (self.min + self.max)/2.0
				mid_i = mid_f.to_i
				@mid = mid_f == mid_i ? mid_i : mid_f
			end
			return @mid
		end

		def range
			return (self.max - self.min)
		end

		def mean
			if @mean.nil?
				mean_f = @data.inject(0, :+)/self.size.to_f
				mean_i = mean_f.to_i
				@mean = mean_f == mean_i ? mean_i : mean_f
			end
			return @mean
		end

		def variance
			if @variance.nil?
				mn = self.mean
				@variance = @data.map { |d| (d-mn)*(d-mn) }
			end
			return @variance
		end

		def stddev
			@stddev = Math.sqrt(self.variance.inject(0, :+)/@data.size.to_f) if @stddev.nil?
			return @stddev
		end

		def median
			@median = Amethyst.median(@data) if @median.nil?
			return @median
		end

		def quartile
			if @quartile.empty?
				ds = @data.size
				mdn = self.median
				if ds.even?
					h1 = @data[0,ds/2]
					h2 = @data[ds/2,ds/2]
					@quartile << Amethyst.median(h1)
					@quartile << mdn
					@quartile << Amethyst.median(h2)
				else
					n, r = ds.divmod 4
					case r
					when 1
						q1_f = (@data[n-1] + 3*@data[n])/4.0
						q3_f = (3*@data[3*n] + @data[3*n+1])/4.0
					when 3
						q1_f = (3*@data[n] + @data[n+1])/4.0
						q3_f = (@data[3*n+1] + 3*@data[3*n+2])/4.0
					else
						raise "this can't happen"
					end
					q1_i = q1_f.to_i
					q3_i = q3_f.to_i
					@quartile << (q1_f == q1_i ? q1_i : q1_f)
					@quartile << mdn
					@quartile << (q3_f == q3_i ? q3_i : q3_f)
				end
			end
			return @quartile
		end

		def iqr
			if @iqr.nil?
				qr = self.quartile
				@iqr = qr.last - qr.first
			end
			return @iqr
		end

		def mode
			if @mode.empty?
				counts = {}
				@data.group_by { |i| i }.each do |k, v|
					counts[v.size] ||= []
					counts[v.size] << k
				end
				@mode.replace counts[counts.keys.max]
			end
			return @mode
		end

		def binwidth
			if @binwidth.nil?
				# Freedman-Diaconis
				@binwidth = 2*self.iqr/Math.cbrt(@data.size)
			end
			return @binwidth
		end

		def histogram(binwidth=nil)
			if binwidth.nil?
				binwidth = self.binwidth
			end
			if @binwidth != binwidth
				@binwidth = binwidth
				@histogram.clear
			end
			if @histogram.empty?
				binrad = binwidth/2
				cur_bin = self.min + binrad
				cur_max = self.min + binwidth
				@histogram << [cur_bin, 0]
				@data.each do |v|
					while v > cur_max
						cur_bin += binwidth
						cur_max += binwidth
						@histogram << [cur_bin, 0]
					end
					@histogram.last[1] += 1
				end
			end
			return @histogram
		end
	end
end

exit unless __FILE__ == $0

if ARGV.include? '--help'
	puts "amethyst #{Amethyst::VERSION} (C) 2014 Giuseppe Bilotta. Licensed under the LGPLv2.1"
	puts "usage: produce data | amethyst [options] [ | gnuplot -p ]"
	puts "options:"
	puts "    --[no-]histogram    enable/disable gnuplot histogram"
	puts "    --[no-]boxplot      enable/disable gnuplot boxplot"
	puts "    --dumb              set gnuplot terminal to dumb"
	exit
end

data = Amethyst::DataSet.new(STDIN.readlines.map { |v| v.chomp.strip.split($;, 2) })

raise "no values" if data.size == 1

# we will produce gnuplot instructions to plot histograms/boxplots if STDOUT is not a tty
# override with --[no-]{histogram,boxplot}
want_histogram = false if ARGV.include?('--no-histogram')
want_histogram = true if ARGV.include?('--histogram')
want_histogram = !STDOUT.tty? if want_histogram.nil?

#  boxplot of the data
want_boxplot = false if ARGV.include?('--no-boxplot')
want_boxplot = true if ARGV.include?('--boxplot')
want_boxplot = !STDOUT.tty? if want_boxplot.nil?

want_plot = (want_histogram || want_boxplot)
multiplot = (want_histogram && want_boxplot)

puts <<END
# count: #{data.size}
# min: #{data.min}
# max: #{data.max}
# mid: #{data.mid}
# range: #{data.range}

# mean: #{data.mean}
# stddev: #{data.stddev}

# mode(s): #{data.mode.sort.join(' ')}

# median: #{data.median}
# quartiles: #{data.quartile.join(' ')}
# IQR: #{data.iqr}
END

exit unless want_plot

# set gnuplot terminal to dumb, filling the screen, if --dumb is specified on the command line
puts "set term dumb size #{`tput cols`.chomp} #{`tput lines`.chomp}" if ARGV.include?('--dumb')

puts <<END if multiplot
set multiplot layout 2, 1

set lmargin at screen 0.05
set rmargin at screen 0.95
set bmargin at screen 0.25
set tmargin at screen 0.95
END

# common to both plots, regardless of the other
puts <<END
unset key
set xrange [#{data.min*0.9}:#{data.max*1.1}]

set ytics nomirror
END

puts "set xtics format \"%.2s%c\"" if data.min.abs*1000 < 1 or data.max.abs/1000 > 1

bins = (data.range / data.binwidth).round
tics = bins
tics/= 2 while tics > 10


puts <<END if want_histogram
set border 3

set yrange [*:*]
set xtics nomirror
set xtics #{data.min + data.binwidth/2}, #{(bins/tics)*data.binwidth}

set style data boxes
set boxwidth #{data.binwidth}

plot '-'
#{data.histogram.map { |p| p.join('  ') }.join("\n")}
e
END

# we're done if we don't want a boxplot
exit unless want_boxplot

# outlier ranges
outlier_minmin = data.quartile.first - 3*data.iqr
outlier_min = data.quartile.first - 1.5*data.iqr
outlier_max = data.quartile.last + 1.5*data.iqr
outlier_maxmax = data.quartile.last + 3*data.iqr

# far outliers
far_out = []
# close outliers
close_out = []

# outliers <
pre_out = []
# outliers >
post_out = []

# min/max for box plot
box_min = nil
box_max = nil
data.data.each do |v|
	if v < outlier_minmin or v > outlier_maxmax
		far_out << v
		if v < outlier_minmin
			pre_out << v
		else
			post_out << v
		end
	elsif v < outlier_min or v > outlier_max
		close_out << v
		if v < outlier_min
			pre_out << v
		else
			post_out << v
		end
	elsif box_min.nil?
		box_min = v
		box_max = v
	elsif v > box_max
		box_max = v
	end
end

if multiplot

	puts <<END
set bmargin at screen 0.05
set tmargin at screen 0.25
unset xtics
END

else

	# boxplot-specific settings if we are the only plot
	puts <<END
set xtics nomirror
set xtics (#{box_min}, #{box_max})
set xtics add (#{data.quartile.join(', ')})
END

	# if there are only a few outliers, tic them
	# otherwise manually define ranges to add

	# range of the pre- and post- outliers
	pre_w = box_min - data.min
	post_w = data.max - box_max

	# percent of tics allowed
	pre_pct = pre_w*10/data.range
	post_pct = post_w*10/data.range

	if pre_out.size > 0
		if pre_pct >= pre_out.size
			puts "set xtics add (#{pre_out.join(', ')})"
		else
			from = pre_out.min
			to = pre_out.max
			step = (to - from)/pre_pct
			puts "set xtics add #{from},#{step},#{to}"
		end
	end

	if post_out.size > 0
		if post_pct >= post_out.size and post_out.size > 0
			puts "set xtics add (#{post_out.join(', ')})"
		else
			from = post_out.min
			to = post_out.max
			step = (to - from)/post_pct
			puts "set xtics add #{from},#{step},#{to}"
		end
	end

end

puts <<END
# outlier cut-offs: #{outlier_minmin} #{outlier_min} #{outlier_max} #{outlier_maxmax}

set border #{multiplot ? 2 : 1 }

unset ytics
set yrange #{multiplot ? "[-1:1.5]" : "[-1:1]"}

set style data boxxyerrorbars

set bars 0

# line
# box
# whiskers and median bars
# close outliers
# far outliers
plot '-' using 1:(0) with lines ls 1, \\
     '-' ls 1, \\
     '-' ls 1, \\
     '-' using 1:(0) with points ls 1 pt 7 ps 1, \\
     '-' using 1:(0) with points ls 1 pt 7 ps 2
#{box_min}
#{box_max}
e
#{data.median} 0 #{data.quartile.first} #{data.quartile.last} -0.5 0.5
e
#{box_min} 0 #{box_min} #{box_min} -0.5 0.5
#{data.median} 0 #{data.median} #{data.median} -0.5 0.5
#{box_max} 0 #{box_max} #{box_max} -0.5 0.5
e
#{close_out.join("\n")}
e
#{far_out.join("\n")}
e
END

