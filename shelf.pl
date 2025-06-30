#!/usr/bin/perl
use v5.40;

my $default_outdir = "..";
my $manifest_filename = "MANIFEST";

sub create_dir_if_not_exists {
    my $path = $_[0];
    if (-e $path) {
        print "INFO: '$path' already exists\n";
    } else {
        print "MKDIR: Created path $path\n";
        mkdir $path;
    }
}
sub create_link_if_not_exists {
    my ($in, $out) = @_;
    if (-e $out) {
        print "INFO: '$out' already exists\n";
    } else {
        print "LINK: '$out' -> '$in'\n";
        symlink $in, $out; 
    }
}

sub install_tree;
sub install_tree {
    my ($inpath, $outpath) = @_;
    opendir my $dir, $inpath or die;
    while (readdir $dir) {
        next if $_ eq "." or $_ eq "..";
        my $item_inpath = $inpath."/".$_;
        my $item_outpath = $outpath."/".$_;
        if (-d $item_inpath) {
            create_dir_if_not_exists $item_outpath;
            install_tree $item_inpath, $item_outpath;
        } else {
            create_link_if_not_exists $item_inpath, $item_outpath;
        }
    }
    closedir $dir or die;
}

sub install_package {
    my $pkg = $_[0];
    my $pkg_out = $default_outdir;
    $pkg_out = `readlink -f $pkg_out` or die;
    chomp $pkg_out;
    print "=== Installing package '$pkg' in folder '$pkg_out' ===\n";
    $pkg = `readlink -f $pkg` or die;
    chomp $pkg;
    # Traverse the whole directory tree
    install_tree $pkg, $pkg_out;
}

open my $manifest, "<", $manifest_filename or die;

while (my $line = <$manifest>) {
    chomp $line;
    install_package $line;
}

close $manifest or die "COULD";

