
# Don't try fancy stuff like debuginfo, which is useless on binary-only
# packages. Don't strip binary too
# Be sure buildpolicy set to do nothing
%define        __spec_install_post %{nil}
%define          debug_package %{nil}
%define        __os_install_post %{_dbpath}/brp-compress

Summary: Token Translation Service
Name: tts
Version: 0.1.0
Release: 1
License: ASL 2.0
Group: Applications/Internet
Source: %{name}-%{version}-bin.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
Requires: jq >= 1.5, curl >= 7.29

%description
%{summary}

%prep
%setup -q

%build
# Empty section.

%pre
/usr/bin/getent passwd tts || /usr/sbin/useradd -r -d /usr/local/bin/tts -s /sbin/nologin tts

%post
/usr/bin/chown -R tts:tts /usr/local/lib/tts

%install
rm -rf %{buildroot}
mkdir -p  %{buildroot}

# in builddir
cp -a * %{buildroot}

%files
/usr/lib/systemd/system/tts.service
/usr/local/bin/tts
/usr/local/bin/ttsc.sh
/usr/local/lib/tts
/var/log/tts
