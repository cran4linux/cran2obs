#
# Spec file for package {{packname}}
# This file is auto-generated using information in the package source,
# esp. Description and Summary. Improvements in that area should be
# discussed with upstream.
#
# Copyright (c) {{year}} SUSE LINUX GmbH, Nuernberg, Germany.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.
# 
# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

%global packname  {{packname}}
%global rlibdir   %{_libdir}/R/library

Name:           R-%{packname}
Version:        {{version}}
Release:        0
Summary:        {{summary}}
Group:          Development/Libraries/Other
License:        {{license}}
URL:            http://cran.r-project.org/web/packages/%{packname}
Source:         {{source0}}
Requires:       R-base
{{depends}}

# %%if 0%%{?sle_version} > 120400 || 0%%{?is_opensuse}
# # Three others commonly needed
# BuildRequires:  tex(ae.sty)
# BuildRequires:  tex(fancyvrb.sty)
# BuildRequires:  tex(inconsolata.sty)
# BuildRequires:  tex(natbib.sty)
# %else
# BuildRequires:  texlive
# %endif
# BuildRequires:  texinfo
BuildRequires:  fdupes
BuildRequires:  R-base
{{builddepends}}
{{needscompilation}}

{{suggests}}
%description
{{description}}

%prep
%setup -q -c -n %{packname}

%build

%install
mkdir -p %{buildroot}%{rlibdir}
%{_bindir}/R CMD INSTALL -l %{buildroot}%{rlibdir} %{packname}
test -d %{packname}/src && (cd %{packname}/src; rm -f *.o *.so)
rm -f %{buildroot}%{rlibdir}/R.css
%fdupes -s %{buildroot}%{rlibdir}

#%%check
#%%{_bindir}/R CMD check %%{packname}

%files
%dir %{rlibdir}/%{packname}

%changelog
