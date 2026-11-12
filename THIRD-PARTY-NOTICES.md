# Third-party notices

scala-commons is licensed under the MIT License (see [LICENSE](LICENSE)). The files listed below contain code
derived from third-party projects, used under their respective licenses.

## xjb

* Project: <https://github.com/xjb714/xjb>
* Copyright 2026 xjb714 and contributors
* License: Apache License, Version 2.0 — <http://www.apache.org/licenses/LICENSE-2.0>
* Files (each carries a header describing the scope of the port and the changes made):
  * `core/src/main/scala/com/avsystem/commons/serialization/json/XjbDouble.scala`
  * `core/src/main/scala/com/avsystem/commons/serialization/json/XjbFloat.scala`

These files port the scalar numeric core of xjb's shortest-decimal float-to-string algorithm to Scala. They are
modified relative to the original (C++ / SIMD) sources; see the file headers for details.

Unless required by applicable law or agreed to in writing, software distributed under the Apache License, Version 2.0
is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
License at the URL above for the specific language governing permissions and limitations.

## FastDoubleParser

* Project: <https://github.com/wrandelshofer/FastDoubleParser>
* Copyright (c) 2024 Werner Randelshofer, Switzerland.
* License: MIT License (reproduced below and in the file headers)
* Files:
  * `core/src/main/scala/com/avsystem/commons/serialization/json/EiselLemireDouble.scala`
  * `core/src/main/scala/com/avsystem/commons/serialization/json/EiselLemireFloat.scala`

These files port FastDoubleParser's Eisel-Lemire fast-path parsing (with the Mushtak-Lemire "without fallback"
refinement) to Scala, with modifications described in the file headers.

```
MIT License

Copyright (c) 2024 Werner Randelshofer, Switzerland.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
