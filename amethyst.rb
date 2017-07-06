#!/usr/bin/ruby

=begin
AMETHYST is a Ruby library/script to do some basic statistical analysis
on series of numbers.

As a Ruby library, it provides an Amethyst::DataSet class which exposes
a number of methods corresponding to basic statistical properties of
a dataset.

As a script, it can be used as a filter from the command line to extract
essential statistics about series of values. In script mode, it can also
provide gnuplot commands to display a nice plot about the mentioned values.
These can be forced with the --histogram and --boxplot command-line parameters,
and are automatically enabled if the output is redirected (disable with --no-histogram
--no-boxplot).

Copyright (C) 2014-2017 Giuseppe Bilotta

Licensed under the GNU Lesser General Public License, version 2.1
See COPYING for details.
=end

=begin
TODO:

* provide an --inline option to read multiple values per line (assume no comments)
* provide a --round option to round values to a given number of digits
* improve from, to and step for outliers in boxplot

=end

module Amethyst
	VERSION=1.1

	# find the median of an array of sorted data
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

		# add single datum w/ comment
		def add_datum(val, comment=nil)
			vi = val.to_i
			vf = val.to_f
			@data << (vi == vf ? vi : vf)
			@comments << (comment ? comment : val)
		end

		# return the value/comment pairs for which the comment matches
		# a specific pattern
		def comment_values(pattern)
			indices = @comments.size.times.select { |i| comments[i].match(pattern) }
			indices.map { |i| [@data[i], @comments[i]] }
		end

		def invalidate
			@size = @data.size

			@min = @max = nil
			@mid = nil
			@mean = nil
			@variance = nil
			@stddev = nil
			@median = nil
			@quartile.clear
			@iqr = nil
			@mode.clear
			@binwidth = nil
			@histogram.clear
		end

		# Data in a DataSet is composed of values and comments
		# It can be initialized either from an array of values (no comments in each)
		# or from an array of pairs (copied to the internal data) or from another dataset
		def initialize(from=nil)

			# set these here, invalidate will only clean them up
			@quartile = []
			@mode = []
			@histogram = []

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

			self.invalidate

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

		# find percentile rank of value
		def rank(val)
			# find index of value
			idx = @data.find_index(val)
			if idx.nil?
				# value not found, so find index of last element smaller than value,
				# (computed as one less than the index of the first element larger than
				idx = @data.find_index { |cmp| cmp > val }
				idx = @data.size if idx.nil? # not found, go to the last
				idx -= 1 # we want the index of the largest element smaller than value!
			end
			# 0-based vs 1-based, hence +1 (yes, we're subtracting 1 to add 1 in the previous
			# conditional block, I know
			return (idx + 1)*100/@data.size
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

		# inter-quartile range
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

		BIN_METHODS = %w/sqrt Sturge Rice Scott Freedman-Diaconis/

		## Bin width methods
		def named_binwidth(name)
			sname = name.to_s.downcase.sub('-','_').intern;
			case sname
			when :sqrt # Square root
				Math.sqrt(@data.size)
			when :sturge # Sturge
				Math.log2(@data.size).ceil + 1
			when :rice # Rice
				2*Math.cbrt(@data.size).ceil
			when :scott # Scott
				7*self.stddev/(2*Math.cbrt(@data.size))
			when :fd, :freedman_diaconis
				2*self.iqr/Math.cbrt(@data.size)
			else
				raise ArgumentError.new("unknown bin width method #{name} (#{sname})")
			end
		end

		# width of bins in histogram
		def binwidth
			if @binwidth.nil?
				@binwidth = named_binwidth(:fd)
			end
			return @binwidth
		end

		def binwidth=(spec)
			oldbw = @binwidth
			case spec
			when String, Symbol
				@binwidth = named_binwidth(spec)
			when Numeric
				@binwidth = spec
			else
				raise ArgumentError.new("unknown bin width specification type #{spec.class.name}")
			end
			@histogram.clear if @binwidth != oldbw
		end

		def histogram(binwidth=nil)
			if not binwidth.nil?
				# interpret binwidth as a possible string spec
				self.binwidth = binwidth
			end
			# set to the actual (interpreted) value
			binwidth = self.binwidth
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

if __FILE__ == $0

	if ARGV.include? '--help'
		puts "amethyst #{Amethyst::VERSION} (C) 2014-2016 Giuseppe Bilotta. Licensed under the LGPLv2.1"
		puts "usage: produce data | amethyst [options] [ | gnuplot -p ]"
		puts "options:"
		puts "    --rank <value>      find percentile rank of value"
		puts "    --[no-]histogram    enable/disable gnuplot histogram"
		puts "    --binwidth <width>  set the bin width by value or by name (supported: #{Amethyst::DataSet::BIN_METHODS.join(' ')})"
		puts "    --[no-]boxplot      enable/disable gnuplot boxplot"
		puts "    --dumb              set gnuplot terminal to dumb, filling the console winow"
		puts "    --term <spec>       set gnuplot terminal to the given <spec>"
		puts "    --plot, -p          call gnuplot ourselves"
		puts "    --plot-code <code>  add the given code to the gnuplot commands"
		exit
	end

	data = Amethyst::DataSet.new(STDIN.readlines.map { |v| v.chomp.strip.split($;, 2) })

	raise "no values" if data.size == 1

	want_rank = ARGV.rindex('--rank')
	if want_rank
		to_rank = ARGV[want_rank+1]
		values_to_rank = [[Integer(to_rank), nil]] rescue data.comment_values(Regexp.new(to_rank, Regexp::IGNORECASE))
	end

	# look for the _last_ occurrence of --term and --dumb
	is_dumb = ARGV.rindex('--dumb')
	has_term = ARGV.rindex('--term')

	# gobble the --term specification, if any
	if has_term
		gnuplot_term = ARGV[has_term+1]
	end

	# if both a --dumb and --term spec were used, the second wins
	if is_dumb and has_term
		if has_term > is_dumb
			is_dumb = nil
		else
			has_term = nil
		end
	end

	# in the end, we're going to use a dumb terminal
	if is_dumb
		gnuplot_term = "dumb size #{`tput cols`.chomp} #{`tput lines`.chomp}"
	end

	# pipe to gnuplot ourselves for --plot or -p, unless stdout has already been redirected
	if !$stdout.tty?
		call_gnuplot = false
	else
		# if a specific term or --plot or -p were given, call gnuplot
		if gnuplot_term or ARGV.include?('--plot') or ARGV.include?('-p')
			call_gnuplot = "gnuplot -p"
		end
	end

	if call_gnuplot
		gnuplot = IO.popen(call_gnuplot, "w")
		$stdout.reopen(gnuplot)
	end

	# we will produce gnuplot instructions to plot histograms/boxplots if STDOUT is not a tty
	# override with --[no-]{histogram,boxplot}
	want_histogram = false if ARGV.include?('--no-histogram')
	want_histogram = true if ARGV.include?('--histogram')
	want_histogram = !$stdout.tty? if want_histogram.nil?

	#  boxplot of the data
	want_boxplot = false if ARGV.include?('--no-boxplot')
	want_boxplot = true if ARGV.include?('--boxplot')
	want_boxplot = !$stdout.tty? if want_boxplot.nil?

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
# outlier fences: #{data.quartile.first - 3*data.iqr/2} #{data.quartile.last + 3*data.iqr/2}
# Bin widths:
#   Square root:       #{data.named_binwidth(:sqrt)}
#   Sturge:            #{data.named_binwidth(:sturge)}
#   Rice:              #{data.named_binwidth(:rice)}
#   Scott:             #{data.named_binwidth(:scott)}
#   Freedman-Diaconis: #{data.named_binwidth(:fd)}
END
	binwidth_spec = ARGV.rindex('--binwidth')
	if binwidth_spec
		binwidth_spec = ARGV[binwidth_spec+1]
		# Check if it's a number, use it as such
		binwidth_spec = Float(binwidth_spec) rescue binwidth_spec
		puts "# Requested bin width: #{binwidth_spec}"
		data.binwidth = binwidth_spec
		puts "# Using bin width: #{data.binwidth}"
	end

	if want_rank
		values_to_rank.each do |pair|
			value = pair.first
			comment = pair.last
			rank = data.rank value
			if comment and not comment.empty?
				puts "# rank for #{comment} (#{value}): #{rank}%"
			else
				puts "# rank for #{value}: #{rank}%"
			end
		end
		puts "# no values matching #{to_rank}" if values_to_rank.empty?
	end

	exit unless want_plot

	# indices of --plot-code
	plot_code_idx = ARGV.each_index.select { |i| ARGV[i] == '--plot-code' }
	plot_code = plot_code_idx.map { |i| ARGV[i+1] }

	# set gnuplot terminal to dumb, filling the screen, if --dumb is specified on the command line
	puts "set term #{gnuplot_term}" if gnuplot_term

	unless plot_code.empty?
		puts "# user-specified plot code"
		puts plot_code.join("\n")
		puts "# user-specified plot code END"
	end

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

set ytics nomirror out
set xtics out
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
		elsif v < box_min
			box_min = v
		end
	end

	throw NotImplementedError if box_min.nil?

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

	boxplot_components = [
		["'-' using 1:(0) with lines ls 1", [box_min, box_max] ], # line from box_min to box_max
		["'-' ls 1", ["#{data.median} 0 #{data.quartile.first} #{data.quartile.last} -0.5 0.5"] ], # box from Q1 to Q2
		["'-' ls 1", [	"#{box_min} 0 #{box_min} #{box_min} -0.5 0.5",
								 "#{data.median} 0 #{data.median} #{data.median} -0.5 0.5",
								 "#{box_max} 0 #{box_max} #{box_max} -0.5 0.5"] ], # lines at box_min, median and box_max
		["'-' using 1:(0) with points ls 1 pt 7 ps 1", close_out ], # close outliers
		["'-' using 1:(0) with points ls 1 pt 7 ps 2", far_out ] # far outliers
	].delete_if { |bc| bc.last.empty? }

	puts <<END
# outlier cut-offs: #{outlier_minmin} #{outlier_min} #{outlier_max} #{outlier_maxmax}

set border #{multiplot ? 2 : 1 }

unset ytics
set yrange #{multiplot ? "[-1:1.5]" : "[-1:1]"}

set style data boxxyerrorbars

set bars 0

plot	#{boxplot_components.map { |bc| bc.first }.join(", \\\n\t")}
#{boxplot_components.map { |bc| bc.last.join("\n") }.join("\ne\n")}
e

END

	if call_gnuplot
		puts "quit"
		$stdout.close
		gnuplot.close
	end

end
