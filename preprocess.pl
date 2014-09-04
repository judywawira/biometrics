#!/usr/bin/perl

use String::Nysiis qw(nysiis);

$in_fld_spec = 'in_fields.cfg'; # file with input field order
$out_fld_spec = 'out_fields.cfg'; # file with output field order
$file = 'junk'; # data file
%fh = (); # input field hash
@ofa = (); # output field array
$k = 0; # generic counter

# if no data file supplied on the command line, then print
# proper usage and exit.
if (!$ARGV[0]) {
    die "\nUsage: $0 {data file} > output_file\n\n";
} else {
    $file = $ARGV[0];
}

# read record field specification for the input data file to be cleaned
open (IFS, "$in_fld_spec") or die "Can't open $in_fld_spec: $!\n";

while(<IFS>) {
    s/\s+$//;
    # push @fs, ($_);
    $fh{$_} = $k++;
}


# read record field specification for output file to be written
open (OFS, "$out_fld_spec") or die "Can't open $out_fld_spec: $!\n";

while(<OFS>) {
    s/\s+$//;
    push @ofa, ($_);
}

#######################################
# Process the data file to be cleaned
#######################################

# print header row
print join("|", @ofa), "\n";

open (I, "$file") or die "Can't open $file: $!\n";

while ($raw = <I>) {

    $raw =~ s/\s+$//;
    @rec = split/\|/,$raw,-1;

    # clean all 
    for $k (0 .. $#rec) {
	$rec[$k] = &basic_fix($rec[$k]);
	}
    
    # skip invalid records 
    next if(&is_invalid_rec(\@rec));

    ################################
    # specific rules for each field
    ################################

    # clean SSN
    if ($fh{ssn}) {
	$rec[ $fh{'ssn'} ] = &clean_ssn($rec[ $fh{'ssn'} ]);
    }

    # clean street address
    if ($fh{str}) {
	$rec[ $fh{'str'} ] = &clean_adr($rec[ $fh{'str'} ]);
    }

    # clean last name
    if ($fh{'ln'}) {
	$rec[ $fh{'ln'} ] = &clean_ln($rec[ $fh{'ln'} ]);
    }

    # clean first name
    if ($fh{'fn'}) {
	$rec[ $fh{'fn'} ] = &clean_fn($rec[ $fh{'fn'} ]);
    }

    # clean telephone number
    if ($fh{'tel'}) {
	$rec[ $fh{'tel'} ] = &clean_tel($rec[ $fh{'tel'} ]);
    }

    # clean zip code
    if ($fh{'zip'}) {
	$rec[ $fh{'zip'} ] = &clean_zip($rec[ $fh{'zip'} ]);
    }

    # clean dob
    if ($fh{'yb'} and $fh{'db'} and $fh{'mb'}) {
	&clean_dob();
    }

    # clean city
    if ($fh{'city'}) {
	$rec[ $fh{'city'} ] = &clean_city($rec[ $fh{'city'} ]);
    }

    # clean sex
    if ($fh{'sex'}) {
	$rec[ $fh{'sex'} ] = &clean_sex($rec[ $fh{'sex'} ]);
    }

    # clean CC
    if ($fh{'cc'}) {
	$rec[ $fh{'cc'} ] = &clean_cc($rec[ $fh{'cc'} ]);
    }

   #############################################################
   # Output the record according to the output specification
   #############################################################

    @out_rec = ();

    for $z (0 .. $#ofa) {
	
	# Check to see if the field is to undergo phonetic transform.
	# Transform fields are denoted by a 'nys-', then the
	# type of transform, e.g.: nys-ln
	if ($ofa[$z] =~ m/nys-/) {

	    # split to find the field
	    @tok = split/-/,$ofa[$z];
	    
	    push @out_rec, ( nysiis( $rec[ $fh{ $tok[1] } ] ) );
	    
	# Check to see if the field is to be validated for use as a
	# blocking field. Block fields are denoted by a 'blk-', then
	# the field name, e.g.: blk-ln
	} elsif ($ofa[$z] =~ m/blk-/) {
	    
	    # split to find the field
	    @tok = split/-/,$ofa[$z];
	    
	    push @out_rec, ( &block_xform( $rec[ $fh{ $tok[1] } ], $tok[1] ) );

	# Check to see if the field is to be subsetted (use less than
	# all the characters in teh field, e.g., first 2 characters.
	# Denoted by 'sub-#', then the field name, e.g.: sub-0-2-ln
	} elsif ($ofa[$z] =~ m/sub-/) {
	    
	    # split to find the field
	    @tok = split/-/,$ofa[$z];
	    
	    push @out_rec, ( substr( $rec[ $fh{ $tok[3] } ], $tok[1], $tok[2] ) );
	    
	} else {
	    
	    push @out_rec, ($rec[ $fh{ $ofa[$z] } ]);
	    
	}
    }

    print join("|", @out_rec), "\n";

}



##########################
# SUBROUTINES
##########################

# determine whether a record is invalid
sub is_invalid_rec {

    $rec_ref = shift;
    @rec1 = @$rec_ref;
    $invalid = 0; 

    # look at specific fields
    if ( $rec1[ $fh{'fn'} ] eq 'MILK' or
	 $rec1[ $fh{'fn'} ] eq 'LIVER' or
	 $rec1[ $fh{'fn'} ] eq 'PRESERVATION' or
	 $rec1[ $fh{'ln'} ] eq 'FR' or
	 $rec1[ $fh{'ln'} ] eq 'DONOR' or
	 $rec1[ $fh{'ln'} ] eq 'HS' or
	 $rec1[ $fh{'ln'} ] eq 'CYPC' or
	 $rec1[ $fh{'ln'} ] eq 'COVANCE' or
	 $rec1[ $fh{'ln'} ] eq 'CAPMHCHEM' or
	 $rec1[ $fh{'ln'} ] eq 'PROTOCOLA' or
	 $rec1[ $fh{'ln'} ] eq 'MHCDC' or
	 $rec1[ $fh{'ln'} ] eq 'CAPCWSURVEY' or
	 $rec1[ $fh{'ln'} ] eq 'MMGCLINICALRESEARCH' or
	 $rec1[ $fh{'ln'} ] eq 'CAPCNSURVEY' or
	 $rec1[ $fh{'ln'} ] eq 'CPLMICRO' or
	 $rec1[ $fh{'fn'} ] eq 'ISCAT' or
	 $rec1[ $fh{'fn'} ] eq 'CPL' or
	 $rec1[ $fh{'fn'} ] eq 'EPC' or
	 $rec1[ $fh{'ln'} ] =~ m/^CAPCPL/ or
	 $rec1[ $fh{'mi'} ] eq 'NUMBER' ) {
	
	$invalid = 1;
	
    }
    
    return $invalid;
    
}

# standard cleaning of all fields
sub basic_fix {
    $z = shift;
    $z =~ s/[^[:ascii:]]//g; # remove non-ascii
    $z =~ s/[#&"\/^,'`]/ /gsi; # punctuation, etc.
    $z =~ s/\s{2,}/ /gsi; # remove extra space elsewhere 
    $z =~ s/^-//; # leading minus sign
    $z =~ s/\s+$//; # trailing white space
    $z =~ s/^\s+//; # leading white space
    $z = uc($z); # uppercase
    $z = '' if $z eq 'NA'; # remove R-added "NA"
    return $z;
}

# clean SSN
sub clean_ssn {

    $s = shift;
    $s =~ s/[^0-9]//gsi;

    if ( $s eq '000000000' or
	 $s eq '111111111' or
	 $s eq '222222222' or
	 $s eq '333333333' or
	 $s eq '444444444' or
	 $s eq '555555555' or
	 $s eq '666666666' or
	 $s eq '777777777' or
	 $s eq '888888888' or
	 $s eq '999999999' or
	 $s eq '111221111' or
	 $s eq '123123123' or
	 $s eq '111223333' or
	 $s eq '399999999' or
	 $s eq '799999999' or
	 $s eq '899999999' or
	 $s eq '909999999' or
	 $s eq '969999999' or	
	 $s eq '123456789' or
	 $s =~ m/00000000/ or
	 $s =~ m/11111111/ or
	 $s =~ m/22222222/ or
	 $s =~ m/33333333/ or
	 $s =~ m/44444444/ or
	 $s =~ m/55555555/ or
	 $s =~ m/66666666/ or
	 $s =~ m/77777777/ or
	 $s =~ m/88888888/ or
	 $s =~ m/99999999/ or
	 length($s) != 9 ) {

	$s = '';
	
    }

    return $s;

}

# clean address string
sub clean_adr {

    $a = shift;
    $a =~ s/\s{2,}/ /gsi; # remove extra space
    $a =~ s/P O /PO /; # fix P O representation

    if ( $a eq 'WMH MEDICAL RECORDS' or
	 $a eq 'KATRINA VICTIM' or
	 $a eq 'KATRINA EVACUEE' or 
	 $a eq 'BAD ADDRESS' or
	 $a eq 'GENERAL DELIVERY' or
	 $a eq 'UNKNOWN' or
	 $a eq 'WISHARD ER' or
	 $a eq 'PATHOLOGY CAP SURVEY' or
	 $a eq 'MARION CO HOME' or
	 $a eq 'EEXEEXEEXEEXEEXEEXEEX' or
	 $a eq '0' or
	 $a eq '1' or
	 $a eq 'UN' or
	 $a eq 'UNK' or
	 $a eq 'UKN' or
	 $a eq 'UNK UNK' or
	 $a eq 'UNKNOWN' or
	 $a eq 'X' or
	 $a eq '0' or
	 $a eq 'GENERAL DELIVERY' or
	 $a eq 'UNKNOWN ADDRESS' or
	 $a eq 'NONE' or
	 $a eq 'UNKN' or
	 $a eq 'N A' or
	 $a eq 'XX' or
	 $a eq 'HOMELESS' or
	 $a eq '***' or
	 $a =~ !(m/[A-Za-z]/) or # if no alph chars, nullify
	 $a eq 'XXX' ) {
	
	$a = '';
	
    }

    return $a;

}

# clean last name
sub clean_ln {
    
    $l = shift;
    $l =~ s/^-{1,}//; # remove dashed from the beginning
    $l =~ s/JR$//;  # remove JR from end
    $l =~ s/SR$//;  # remove SR from end
    $l =~ s/III$//; # remove III from end
    $l =~ s/\s//gsi; # remove all spaces from name

    if ( $l eq 'WMH' or
	 $l eq 'ISCAT' or
	 $l eq 'REUSE' or
	 $l eq 'USE' or
	 $l eq 'TEST' or
	 $l eq 'ERX-NUMBER' or
	 $l eq 'FR' or
	 $l eq 'ER' or
	 $l eq 'LAB' or
	 $l eq 'UNKNOWN' or
	 $l eq 'UNK' or
	 $l eq 'JANE DOE' or
	 $l eq 'PATIENT' or
	 $l eq 'DOG' or
	 $l eq 'RADIOLOGY' or
	 $l eq 'JAIL' or
	 $l eq 'UNKN' or
	 $l eq 'EMP' or
	 $l eq 'PID' or
	 $l eq 'MHCAP' or
	 $l eq 'EGT' or
	 $l eq 'TEST' or
	 $l eq 'DONOR' or
	 $l eq 'EHWR' or
	 $l eq 'CHANGED' or
	 $l eq 'XGCR' or
	 $l eq 'RESEARCHID' or
	 $l eq 'EMPLOYEE' or
	 $l eq 'XLIS' or
	 $l eq 'CPLSEROLOGY' or
	 $l eq 'RESEARCH' or
	 $l eq 'JOHN DOE' or
	 $l eq 'JANE DOE' or
	 $l eq 'PATIENT' or
	 $l eq 'ANIMAL' or
	 $l eq 'ELILILLY' or
	 $l eq 'PRESERVATION' or
	 $l eq 'UNK' or
	 $l eq 'UNKNOWN' or
	 $l eq 'IDX' or
	 $l eq 'NONE' or
	 $l eq 'DUMMY' or
	 $l eq 'TEST-PT' or
	 $l eq 'TEST-PATIENT' or
	 $l eq 'UNKN' or
	 $l eq 'JAIL' or
	 $l eq 'NA' or
	 $l eq 'N A' or
	 $l eq 'WRONG' or
	 $l eq 'SUBJECT' or
	 $l eq 'SCREENING' or
	 $l eq 'MOLECULAR2009' or
	 $l eq 'EXPO' or
	 $l eq 'PROTOCOL' or
	 $l eq 'PIG' or
	 $l eq 'NONAME' or
	 $l eq 'UNIDENTIFIED' or
	 $l eq 'UNIDENTIFED' or
	 $l eq 'AAA' or
	 $l eq 'CHECK-NAME' or
	 $l eq 'PORCINE' or
	 $l eq 'STUDYID' or
	 $l =~ m/STUDY[0-9]/ or
	 $l =~ !(m/[A-Za-z]/) ) {
	
	return '';
	
    }

    # remove prefixes
    $l =~ s/^LAB-//;
    $l =~ s/^ACHEK//;
    $l =~ s/^ZZZ//;
    $l =~ s/[0-9]//gsi;
    $l =~ '' if ( length($l) < 2 );

    return $l;
    
}

# clean first names
sub clean_fn {
    $f = shift;
    $f =~ s/^-//; # remove dashes from beginning
    $f =~ s/-$//; # remove dashes from end
    $f =~ s/ JR$//; # remove JR from end
    $f =~ s/ SR$//; # remove SR from end
    $f =~ s/ III$//; # remove III from end
    $f =~ s/\d{1,}$//; # remove digits from end of first name
    $f =~ s/\s//gsi; # remove all spaces from name
    
    if ( $f eq 'INF' or
	 $f eq 'TEMP' or
	 $f eq 'INFANT' or
	 $f eq 'PATIENT' or
	 $f eq 'NOT' or
	 $f eq 'QC' or
	 $f eq 'TEMPORARY' or
	 $f eq 'HOSP' or
	 $f eq 'MALE' or
	 $f eq 'UNIDENTIFIED' or
	 $f eq 'JAIL' or
	 $f eq 'NONE' or
	 $f eq 'INFANT' or
	 $f eq 'BOY' or
	 $f eq 'GIRL' or
	 $f eq 'INF' or
	 $f eq 'BABY' or
	 $f eq 'BABYBOY' or
	 $f eq 'BABYGIRL' or
	 $f eq 'TO' or
	 $f eq 'MILK' or
	 $f eq 'LIVER' or
	 $f eq 'TEST' or
	 $f eq 'PATIENT' or
	 $f eq 'UNK' or
	 $f eq 'UNKNOWN' or
	 $f eq 'RSIU' or
	 $f eq 'NONE' or
	 $f eq 'VIROLOGY' or
	 $f eq 'UNKN' or
	 $f eq 'JAIL' or
	 $f eq 'NAME' or
	 $f eq 'INF-BOY' or
	 $f eq 'INF-GIRL' or
	 $f eq 'OFF' or
	 $f eq 'DUP' or
	 $f eq 'DUPL' or
	 $f eq 'LIS' or
	 $f eq 'INFUSATE' or
	 $f eq 'EMPLOYEE' or
	 $f eq 'SUBJECT' or
	 $f eq 'PERF' or
	 $f eq 'SISTER' or
	 $f eq 'DONOR' or
	 $f eq 'FLUID' or
	 $f eq 'SOURCE' or
	 $f eq 'EMP' or
	 $f eq 'GIR' or
	 $f eq 'USE' or
	 $f eq 'NBSINFANT' or
	 $f eq 'INFGIRL' or
	 $f eq 'INFBOY' or
	 $f eq 'INFANTBOY' or
	 $f eq 'INFANTGIRL' or 
	 $f eq 'BLANKNAME' or
	 $f eq '-' or
	 $f =~ m/^SUBJ/ or
	 $f =~ m/^SPIUPUI/ or
	 $f =~ m/^SPM/ or
	 $f =~ m/^HCV/ or
	 $f =~ m/^POSTOP/ or
	 $f =~ m/^0/ or
	 length($f) < 2 ) {
	
	$f = '';
	
    }
    
    $f =~ s/[0-9]//gsi; # remove numbers
    $f =~ s/^BABY// if (length($f) > 4 and $f =~ m/^BABY/);
    $f = '' if (length($f) < 2);
    
    return $f;
    
}

# clean telephone
sub clean_tel {
    
    $t = shift;
    $t =~ s/[-()]\s//gsi; # Remove dash, parentheses and spaces

    if ( $t =~ m/0000000/ or
	 $t =~ m/1111111/ or
	 $t =~ m/2222222/ or
	 $t =~ m/3333333/ or
	 $t =~ m/4444444/ or
	 $t =~ m/5555555/ or
	 $t =~ m/6666666/ or
	 $t =~ m/7777777/ or
	 $t =~ m/8888888/ or
	 $t =~ m/9999999/ or
	 $t eq '3176386585' or # Salvation Army Adult Rehab
	 $t eq '3176939222' or # Pleasant run childrens home
	 $t eq '3176305215' or # resolute residential treatment program
	 $t eq '3179210836' or # Office of refugee resettlement
	 $t eq '3174238909' or # Horizon house
	 length($t) < 7 or
	 $t !~ m/[0-9]/ ) {
	
	$t = '';
	
    }
    
    return $t;
    
}


# clean zip
sub clean_zip {
    
    $z = shift;
    $z =~ s/[^0-9]//gsi;
    $z = substr($z,0,5);
    
    if ( $z eq '00000' or
	 $z eq '11111' or
	 $z eq '22222' or
	 $z eq '33333' or
	 $z eq '44444' or
	 $z eq '55555' or
	 $z eq '66666' or
	 $z eq '77777' or
	 $z eq '88888' or
	 $z eq '99999') {
	
	$z = '';
	
    }
    
    return $z;
    
}

# clean dob
sub clean_dob {
    
    
    
# If YB is one or two digits
    if ( length( $rec[ $fh{'yb'} ] ) <= 2) {
	
	$zb = $yb;
	
	# zero-pad yb	
	$rec[ $fh{'yb'} ] = sprintf("%02d", $rec[ $fh{'yb'} ] );
	
	# 1911 - 1999
	if ( $rec[ $fh{'yb'} ] > 10 ) {
	    $rec[ $fh{'yb'} ] = '19' . $rec[ $fh{'yb'} ];
	
	# 2000 - 2010
	} else {
	    $rec[ $fh{'yb'} ] = '20' . $rec[ $fh{'yb'} ];
	    
	}
	
    }
    
# Remove non-numeric data from data fields
$rec[ $fh{'yb'} ] =~ s/[^0-9]//gsi;
    $rec[ $fh{'mb'} ] =~ s/[^0-9]//gsi;
    $rec[ $fh{'db'} ] =~ s/[^0-9]//gsi;
    
# 01/01/190[0167] are invalid dates 
# keep year of birth as an approximate age
#if ( ( $rec[ $fh{'yb'} ] == 1900 or
#	$rec[ $fh{'yb'} ] == 1901 or 
#	$rec[ $fh{'yb'} ] == 1906 or 
#	$rec[ $fh{'yb'} ] == 1907) and

#	$rec[ $fh{'mb'} ] == 1 and 
#	$rec[ $fh{'db'} ] == 1) {

#	$rec[ $fh{'yb'} ] = '';
#	$rec[ $fh{'mb'} ] = '';
#	$rec[ $fh{'db'} ] = '';

#}

# eliminate future dates and unusually old dates
# keep year of birth as an approximate age
    if ( $rec[ $fh{'yb'} ] > 2010 or ( $rec[ $fh{'yb'} ] < 1880 & $rec[ $fh{'yb'} ] > 1800 ) ) {
	
	$rec[ $fh{'mb'} ] = '';
	$rec[ $fh{'db'} ] = '';
	
    }
    
    1;
    
}

# clean city
sub clean_city {
    
    $c = shift;
    
    if ( $c eq 'UNK' or
	 $c eq 'UNKNOWN' or
	 $c eq 'UNKN' or
	 $c eq 'NONE') {
	
	return '';
	
    }
    
    
    if ($c eq 'INDPLS' or $c eq 'INDNPLS' ) {
	$c = 'INDIANAPOLIS';
    }
    
    return $c;
    
}

# transform field for blockign purposes
sub block_xform {
    
    $val = shift;
    $val_type = shift;
    
    if ($val_type eq 'mb') {
	
	$val = '' if ( $rec[ $fh{'mb'} ] == 1 & $rec[ $fh{'db'} ] == 1 );
	
    } elsif ($val_type eq 'db') {
	
	$val = '' if ( $rec[ $fh{'mb'} ] == 1 & $rec[ $fh{'db'} ] == 1 );
	
    }
    
    return $val;
    
}

# clean sex: allow only M and F
# note: may want to expand this
sub clean_sex {
    
    $sz = shift();
    
    if ($sz eq 'M' or $sz eq 'F') {
	
	return $sz;
	
    } else {
	
	return '';
	
    }
    
}


# clean CC
# the "basic clean" function removes extra spaces, carats, etc.
# This function will nullify field if no alpha characters
sub clean_cc {
    
    $zcc = shift();
    
    if ($zcc !~ m/[A-za-z]/) {
	
	return '';
	
    } else {
	
	return $zcc;
	
    }
    
}
