# Hapoid
A reviewer for your Bahasa Indonesia PO files

## Requirements
- GHC >= 8.1

## Installation
```
git clone https://github.com/wisn/hapoid.git
cd hapoid
chmod +x install.sh
./install.sh
```

## Overview
```
Usage: hapoid COMMAND [PATH] [OPTION] [ARGS]...

Commands
help           Display this message
check          Check id.po file in the current directory
check <path>   Check file in the specified path i.e "check my/path"
               will detected as "my/path/id.po"
about          Display about message


Options
--fuzzy        Review all fuzzy-translations
```

## Progress
### Command Parser
- [DONE] Refactor
- [DONE] Command Parser
- [DONE] Path Finder
- [DONE] Install Shell
- [ONGOING] PO Checker

### Detector
Ongoing...
