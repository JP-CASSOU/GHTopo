GHTopo
======

GHTopo: A cave mapping software

The zip contains GHTopo software, examples and Lazarus source code

INSTALLATION:
=============

GHTopo is a portable application. Unzip into any folder.

Modifying GHTopo's folder contents is not recommended.

GHTopo XTB documents are usually stored in ./Format_GHTopo and ./Format_XML

DOCUMENT FORMATS:
=================

- *.Tab: The TOPOROBOT Tab spread-sheet format ( fields separated by Tab \t ). Use exclusively this format for export from LimeLight. GHTopo can read and write this format.

- *.xtb: GHTopo standard format. Is a *.Tab format with supplementary columns and sections. While conversion to *.Tab format, additional sections and columns will be lost.

- *.Text: Older text format of LimeLight with fixed length of columns. This format is deprecated and read-only by GHTopo

- *.gtx: A new XML format of GHTopo. 
