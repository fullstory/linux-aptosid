#!/usr/bin/ruby -w
# changeset-all.rb - grab udiffs of all changesets in bk from the last X weeks
# This will grab all changesets committed in the past X days, weeks, months,
# or years, storing them in <dir>/new/cset@<cset>@<hash> as unified diffs.
# It will only download changesets that aren't already in
# <dir>/*/*@<hash>.  Note that <cset> numbers may change (thanks bitkeeper!),
# while <hash> will remain constant.
#
#    Copyright (C) 2004-2005  Andres Salomon <dilinger@voxel.net>
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

require 'uri'
require 'date'
require 'fileutils'
require 'net/http'
require 'html/htmltokenizer';

class Tokenize
	def Tokenize.tokenize(url, tags = ['a'])
		page = Net::HTTP.get(URI.parse(url))
		tok = HTMLTokenizer.new(page)

		while tag = tok.getTag(*tags)
			yield(tag.tag_name, tag.attr_hash, tok.getText())
		end
	end
end

class BKWeb
	BASE = 'http://linux.bkbits.net:8080/linux-2.6/'

	def BKWeb.changeset(cset)
		url = BASE + cset

		cs_udiff = cs_author = cs_jd = nil
		Tokenize.tokenize(url, ['a', 'font']) { |tag, attr, text|
			case tag
			when 'a'
				href = attr['href']
				if href =~ /^gnupatch@/
					cs_udiff = BASE + href
				end
			when 'font'
				fields = text.split('&nbsp;')
				cs = date = time = author = ''
				cs = fields.shift until cs != ''
				date = fields.shift until date != ''
				author = fields.shift until author != ''

				if author != nil && cset.split('@').pop == cs.split('@').pop
					cs_jd = date.split(' ').shift
					cs_author = author
				end
			end
		}

		raise "Unable to find changeset #{cset}!" if cs_udiff.nil?
		raise "Error parsing changeset #{cset}'s HTML!" if cs_jd.nil? || cs_author.nil?
		[ cs_udiff, cs_author, cs_jd ]
	end

	def BKWeb.search(expr)
		expr = URI.escape(expr)
		url = BASE + "search/?expr=#{expr}&search=ChangeSet+comments"

		results = {}
		Tokenize.tokenize(url, ['a']) { |tag, attr, text|
			h = attr['href']
			if h =~ /^cset@/
				results[h] = ChangeSet.new(h)
			end
		}
		results
	end

	def BKWeb.last(timeframe)
		raise "Invalid time format '#{timeframe}'!" unless timeframe =~ /^\d+[dwMy]$/
		url = BASE + "ChangeSet@-#{timeframe}"

		results = {}
		Tokenize.tokenize(url, ['a']) { |tag, attr, text|
			h = attr['href']
			if h =~ /^cset@/
				h = h.split('?').shift
				results[h] = ChangeSet.new(h)
			end
		}
		raise 'No changesets found!' if results.length == 0
		results
	end
end

class ChangeSet
	attr_reader :cset

	def _lookup
		@udiff, @author, self.jd = BKWeb.changeset(@cset)
	end

	include Comparable
	def <=>(other)
		self.jd <=> other.jd
	end

# comparison comparing cset numbers instead of dates
#	def <=>(other)
#		a = @cset.split('.')
#		b = other.cset.split('.')
#		until a[0].nil? || b[0].nil?
#			cmp = (b[0].to_i <=> a[0].to_i)
#			return cmp unless cmp == 0
#			a.shift
#			b.shift
#		end
#		b.length <=> a.length
#	end

	def initialize(cset = nil)
		@cset = cset
		@udiff = @author = @jd = nil
	end

	def cset=(cset = nil)
		@cset = cset
		@udiff = @author = @jd = nil
	end

	def jd=(date)
		case date.class.to_s
			when 'Array'
				if date.length != 3
					raise 'Invalid date; length != 3!'
				end
			when 'String'
				d, date = date, date.split('-')
				raise "Invalid date: #{d}!" if date.length != 3
			when 'NilClass'
				@jd = nil
				return @jd
			else
				raise "Invalid date type (#{date.class})!"
		end

		@jd = Date.civil_to_jd(date[0].to_i, date[1].to_i, date[2].to_i)
	end

	# Julian Date
	def jd
		_lookup() if @jd.nil?
		@jd
	end

	def udiff
		_lookup() if @udiff.nil?
		@udiff
	end

	def author
		_lookup() if @author.nil?
		@author
	end
end

if ARGV.length != 2
	$stderr.puts "Usage: #{$0} <dir> <timeframe ([0-9]+[wdMy])>"
	exit(1)
end

FileUtils.mkdir_p(ARGV[0] + '/new')

csets = BKWeb.last(ARGV[1])
puts "#{csets.length} changesets found."

csets.each { |key, val|
	print "cset: #{key}: "
	begin
		hash = val.udiff.split('@').pop
		if Dir.glob(ARGV[0] + '/*/*@' + hash).length > 0
			puts "skipped."
			next
		end
		File.open(ARGV[0] + '/new/' + key + '@' + hash, 'w') { |f|
	 		f.puts Net::HTTP.get(URI.parse(val.udiff))
		}
		puts "new."
	rescue => e
		puts "exception: #{e}"
	end
}

puts 'Done fetching changesets.'
