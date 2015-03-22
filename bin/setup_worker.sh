#!/bin/bash

rm -rf ~/crust
rm -rf ~/cbmc-5.0

mkdir ~/crust 2> /dev/null

sudo yum install -y apr-1.5.0-2.11.amzn1.x86_64 apr-util-1.4.1-4.14.amzn1.x86_64 libserf-1.3.7-1.6.amzn1.x86_64 subversion-libs-1.8.10-1.45.amzn1.x86_64 subversion-1.8.10-1.45.amzn1.x86_64 mpfr-2.4.2-1.7.amzn1.x86_64 libmpc-0.8.2-1.4.amzn1.x86_64 cpp48-4.8.2-16.2.99.amzn1.x86_64 libgomp-4.8.2-16.2.99.amzn1.x86_64 kernel-headers-3.14.34-27.48.amzn1.x86_64 glibc-headers-2.17-55.93.amzn1.x86_64 glibc-devel-2.17-55.93.amzn1.x86_64 gcc48-4.8.2-16.2.99.amzn1.x86_64 gcc-4.8.2-3.19.amzn1.noarch libstdc++48-devel-4.8.2-16.2.99.amzn1.x86_64 gcc48-c++-4.8.2-16.2.99.amzn1.x86_64 gcc-c++-4.8.2-3.19.amzn1.noarch 1:perl-Error-0.17020-2.9.amzn1.noarch perl-TermReadKey-2.30-20.9.amzn1.x86_64 perl-Git-2.1.0-1.35.amzn1.noarch git-2.1.0-1.35.amzn1.x86_64 1:perl-Compress-Raw-Zlib-2.061-4.1.amzn1.x86_64 perl-Data-Dumper-2.145-3.5.amzn1.x86_64 perl-Encode-Locale-1.03-5.8.amzn1.noarch perl-HTML-Tagset-3.20-15.7.amzn1.noarch perl-IO-HTML-1.00-2.5.amzn1.noarch perl-Net-LibIDN-0.12-15.6.amzn1.x86_64 mailcap-2.1.31-2.7.amzn1.noarch perl-LWP-MediaTypes-6.02-2.9.amzn1.noarch perl-IO-Socket-IP-0.21-4.5.amzn1.noarch perl-Business-ISBN-Data-20120719.001-2.7.amzn1.noarch perl-Business-ISBN-2.06-2.7.amzn1.noarch perl-URI-1.60-9.8.amzn1.noarch perl-WWW-RobotRules-6.02-5.12.amzn1.noarch perl-Compress-Raw-Bzip2-2.061-3.11.amzn1.x86_64 perl-IO-Compress-2.061-2.12.amzn1.noarch perl-Mozilla-CA-20110914-2.6.amzn1.noarch 1:perl-TimeDate-2.30-2.7.amzn1.noarch perl-HTTP-Date-6.02-8.8.amzn1.noarch perl-HTTP-Message-6.06-6.10.amzn1.noarch perl-HTML-Parser-3.71-4.7.amzn1.x86_64 perl-HTTP-Daemon-6.01-5.11.amzn1.noarch perl-HTTP-Cookies-6.01-5.12.amzn1.noarch perl-HTTP-Negotiate-6.01-5.12.amzn1.noarch perl-File-Listing-6.04-7.11.amzn1.noarch perl-HTML-Form-6.03-4.3.amzn1.noarch perl-Net-SSLeay-1.65-2.10.amzn1.x86_64 perl-IO-Socket-SSL-1.94-3.13.amzn1.noarch perl-Net-HTTP-6.06-2.11.amzn1.noarch perl-libwww-perl-6.05-2.17.amzn1.noarch perl-LWP-Protocol-https-6.04-2.7.amzn1.noarch perl-LWP-Protocol-http10-6.03-3.6.amzn1.noarch 1:perl-Bundle-LWP-6.00-0.1.amzn1.noarch patch-2.7.1-8.9.amzn1.x86_64 m4-1.4.16-9.10.amzn1.x86_64 bison-2.4.1-5.8.amzn1.x86_64 flex-2.5.36-1.8.amzn1.x86_64 htop-1.0.1-2.3.amzn1.x86_64 perl-File-Next-1.12-1.2.amzn1.noarch ack-2.12-1.2.amzn1.noarch 12:aspell-0.60.6-12.7.amzn1.x86_64 1:emacs-common-23.1-25.18.amzn1.x86_64 hunspell-1.2.8-16.2.amzn1.x86_64 1:emacs-23.1-25.18.amzn1.x86_64 python27-libs-2.7.8-6.75.amzn1.x86_64 python27-2.7.8-6.75.amzn1.x86_64;

cd ~/
tar xf ~/cbmc-5.0.tar.bz2
tar xf ~/crust_worker.tar.bz2 -C ~/crust

mkdir ~/bin 2> /dev/null
mv ~/z3 ~/bin

if ! grep -q "/home/ec2-user/bin" ~/.bashrc; then
	echo 'export PATH=$PATH:/home/ec2-user/bin' >> ~/.bashrc
fi

sudo mkdir /media/ephemeral0/smt_temp 2> /dev/null;
sudo chown ec2-user /media/ephemeral0/smt_temp
