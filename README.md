# PeRseus

Perseus refers to the Perseus Digital Library (http://www.perseus.tufts.edu/hopper/), R stans for R. R also stands for recycling. PeRseus.R is an importer for PDL and OGL texts (https://github.com/OpenGreekAndLatin) that are citable via the CTS/CITE architecture (http://cite-architecture.github.io). It follows the principle that textual research is based on analysis and synthesis, effectively creating new tokenisations of the same text. Often those tokenisations are not saved. PeRseus attempts to save those tokenisations and make them citable, so they can be retrieved and reused (recycled) for new tasks, projects, research questions. The different tokenisations of the texts are saved as R binaries (.rds). PeRseus is written in R and uses git's tree structure and github's api (https://developer.github.com/v3/) to retrieve the generated binaries.The development of PeRseus has just started and is open source (MIT see below).

## The MIT License (MIT)
Copyright (c) 2016 Thomas Koentges

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
