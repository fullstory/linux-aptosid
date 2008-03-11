#!/usr/bin/ruby -w
# dpatchify.rb - add dpatch headers to a patch (or group of patches)
# This will (when supplied w/ foo.patch) create a file called foo.dpatch,
# that contains a dpatch header.  It assumes a standard bitkeeper header
# will be in the original patch file, and fills in the dpatch header info
# from that.  Useful for instantly dpatch-izing large sets of bk backports.
#
#    Copyright (C) 2004  Andres Salomon <dilinger@voxel.net>
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

if ARGV.length < 1
	$stderr.puts "Usage: #{$0} <patch files>"
	exit(1)
end

ARGV.each { |file|
	lines = ''
	File.open(file) { |f| lines = f.readlines }
	author = desc = 'unknown'
	if lines[0] =~ /BitKeeper generated/
		author = lines[3].chomp.split(/\s+/).pop
		desc = lines[4].chomp.sub(/^\s*#\s*/, '')
	else
		$stderr.puts "Warning: #{file} is not a vanilla bitkeeper patch!"
		next
	end
	
	prepend = <<EOS
#! /bin/sh -e
## <PATCHNAME>.dpatch by <PATCH_AUTHOR@EMAI>
##
## All lines beginning with `## DP:' are a description of the patch.
## DP: Description: #{desc}
## DP: Patch author: #{author}
## DP: Upstream status: backported

. $(dirname $0)/DPATCH

@DPATCH@
EOS

	file = file.sub(/\.?(patch|dpatch|diff)?$/, '.dpatch')
	File.open(file, 'w') { |f| f.print prepend; f.print lines }
}
