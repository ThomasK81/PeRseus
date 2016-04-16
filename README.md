# PeRseus

Perseus refers to the Perseus Digital Library (http://www.perseus.tufts.edu/hopper/), R stans for R. R also stands for recycling. PeRseus.R is an importer for PDL and OGL texts (https://github.com/OpenGreekAndLatin) that are citable via the CTS/CITE architecture (http://cite-architecture.github.io). It follows the principle that textual research is based on analysis and synthesis, effectively creating new tokenisations of the same text. Often those tokenisations are not saved. PeRseus attempts to save those tokenisations and make them citable, so they can be retrieved and reused (recycled) for new tasks, projects, research questions. The different tokenisations of the texts are saved as R binaries (.rds). PeRseus is written in R and uses git's tree structure and github's api (https://developer.github.com/v3/) to retrieve the generated binaries.The development of PeRseus has just started and is open source (MIT see below).

### Tokenisations currently generated (R type in brackets)

Text stripped of XML tags, e.g. phi1056/phi001.rds (matrix)
Corpus-based generated dictionary forms (parsing) of text, e.g. phi1056/phi001_parsed.rds (matrix)
All possible dictionary forms for each word: e.g. phi1056/phi001_stem.rds (list)
I/O mistakes with percentage (number of I/O mistakes / number of sentences), e.g. phi001_iobugs_4.rds.
Directory of LDA topic model data, names according to number of topics, number of iterations, and seed. It includes term-topic and document-topic matrixes as well as a D3 visualisation of that model: e.g. phi001_tm_12_5000_37

### Github API

An example how Github's API can help to retrieve the different tokenisations is included in the (unfinished) reader_app.

### The MIT License (MIT)
Copyright (c) 2016 Thomas Koentges

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
