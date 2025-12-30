# RASM Compiler Specification

## Profiles and namespaces

test and main profiles are special profiles, for those profiles the namespace does not include the profile name itself, so a private symbol in main can be seen in the same "file" in test, it does not happen for the custom profiles.
