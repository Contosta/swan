#!/usr/bin/perl
#Eric Morrison
#8/4/15
#Usage: avg_daily_temps.pl - this should be the actual name of the hourly temp file you want compiled to daily averages [input.txt] [output.txt] [errorFile.txt]
#This file takes as input a tab-delimited text file with hourly temperature measurements from SWaN plots. Year, month, day, and time should be specified in the first four columns (with those column headings), followed by one column for each plot with the plot number as the header. The script outputs the average temperature for each day in each plot in a tab-delimited text format.

use strict;
use warnings;

my $in = $ARGV[0];
my $out = $ARGV[1];
my $out2 = $ARGV[2];

open(IN, "$in") || die "Can't open input file.\n";
open(OUT, ">$out") || die "Can't open output file.\n";
open(OUT2, ">$out2") || die "Can't open output for error file.\n";

print OUT2 "Error\tLine#\tPlot#\tError type\n";
chomp(my @in = <IN>);

if(scalar@in ==1)
	{
	$in[0] =~ s/\r|\r\n|\n/\n/g;
	@in = split("\n", $in[0]);
	}

my $headers = shift@in;
my @headers = split("\t", $headers);
my %temp;
my %count;
my %dateCount;
my $dummyID;

#Line counter for error messages
my $u = 1;

#For each line in the data split the line by tabs
foreach my $ent (@in)
	{
	$u++;
#	print $u, "\n";
	my @ent = split("\t", $ent);
	
#For each column in each line, starting at column four, add the temperature value to the running sum and count the number of entries for each Year-Month-Day-Plot
	for(my $i = 4; $i<@ent; $i++)
		{
		if($ent[$i] =~ /^NAN$/i || $ent[$i] =~ /^NA$/i) #If entry is NAN skip it and print a warning
			{
			print OUT2 "Just so you know, line $u plot $headers[$i] has a NAN value.\t$u\t$headers[$i]\t$ent[$i]\n";
			}elsif(length($ent[$i]) == 0){ #If entry is missing skip it and print a warning
			print OUT2 "Just so you know, line $u plot $headers[$i] has no value and is not marked NAN.\t$u\t$headers[$i]\tmissing\n";
			}elsif($ent[$i] !~ /^\d+/ && $ent[$i] !~ /^-\d+/){ #If entry has a value that doe not start with a number or a negative number skip and print a warning
			print OUT2 "Just so you know, line $u plot $headers[$i] has a message other than NAN, missing, numeric.\t$u\t$headers[$i]\t$ent[$i]\n";
			}else{
			#Add to sum
			$temp{$ent[0]."\t".$ent[1]."\t".$ent[2]}{$headers[$i]} += $ent[$i];
			$dummyID = $ent[0]."\t".$ent[1]."\t".$ent[2];
			#Add to count
			$count{$ent[0]."\t".$ent[1]."\t".$ent[2]}{$headers[$i]}++;
			$dateCount{$ent[0]."\t".$ent[1]."\t".$ent[2]} = $count{$ent[0]."\t".$ent[1]."\t".$ent[2]}{$headers[$i]};
			if($count{$ent[0]."\t".$ent[1]."\t".$ent[2]}{$headers[$i]} == 25)
				{
				print $u, "\n";
				}
			}
		}
	}

print OUT "Year\tMonth\tDay\tCount\t";

foreach my $plots (sort {$a cmp $b} keys %{ $temp{$dummyID} } )
	{
	print OUT $plots, "\t";
	}
print OUT "\n";

my $lineCounter = 1;

foreach my $date (sort {$a cmp $b} keys %temp)
	{
	print OUT $date, "\t", $dateCount{$date}, "\t";
	
	$lineCounter++;
	if($dateCount{$date} != 24)
		{
		print $lineCounter, "\n";
		}

	foreach my $id (sort {$a cmp $b} keys %{ $temp{$date} } )
		{
		print OUT $temp{$date}{$id}/$count{$date}{$id}, "\t";
		}
	print OUT "\n";
	}
