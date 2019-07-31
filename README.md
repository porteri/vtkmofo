# vtkmofo
VTK legacy format using modern Fortran

[![AppVeyor build status][AppVeyor build image]](https://ci.appveyor.com/project/porteri/vtkmofo)
[![Travis CI build status][Travis CI build image]](https://travis-ci.org/porteri/vtkmofo)
[![release downloads][download image]](https://github.com/porteri/vtkmofo/releases)
[![codecov][codecov image]](https://codecov.io/gh/porteri/vtkmofo)

This is a generic modern Fortran interface to write a VTK formatted file using the specifications outlined in [version 3.0][VTK Format Link].

Format support:
 - [X] Legacy Format
  * This code fully supports the legacy .vtk file format in ASCII
 - [X] XML Format
   - [ ] Serial Formats
     - [ ] Image Data (.vti)
     - [ ] Poly Data (.vtp)
     - [X] Rectilinear Grid (.vtr)
     - [X] Structured Grid (.vts)
     - [X] Unstructured Grid (.vtu) (Currently under development)
   - [ ] Parallel Formats
     - [ ] Image Data (.pvti)
     - [ ] Poly Data (.pvtp)
     - [ ] Rectilinear Grid (.pvtr)
     - [ ] Structured Grid (.pvts)
     - [ ] Unstructured Grid (.pvtu)

This code is built and tested with the following:

Compilers: (may work for older ones as well)
 - [gfortran][gcc link] 7.4, 8.1, 8.2, 8.3, 9.0
 - [Intel][Intel link] 2018

Operating Systems:
 - Linnux (Tested on Ubuntu 16, 18)
 - Windows (Tested on Windows 8, 10)
 - MacOS

Build System:
 - [CMake][CMake link] 3.12.2 or newer

Examples:
# Structured Grid (2D)
![Cylinder_2d](documents/files/cylinder_image_2d.png?raw=true "Cylinder 2D example")
# Rectilinear Grid
![Cube_3d](documents/files/cube_image.png?raw=true "Cube example")
# Polygonal Data
![Pyramid](documents/files/pyramid_image.png?raw=true "Pyramid example")
# Structured Points
![Prism](documents/files/rectangle_image.png?raw=true "Prism example")
# Unstructured Grid
![T_shape](documents/files/t_shape.png?raw=true "T-shape example")

[Hyperlinks]:#
[AppVeyor build image]: https://ci.appveyor.com/api/projects/status/omlvmn8xcr9sxuwt?svg=true "AppVeyor build badge"
[Travis CI build image]: https://travis-ci.org/porteri/vtkmofo.svg?branch=master "Travis CI build badge"
[download image]: https://img.shields.io/github/downloads/porteri/vtkmofo/total.svg?style=flat-square "Download count badge"
[codecov image]: https://codecov.io/gh/porteri/vtkmofo/branch/master/graph/badge.svg
[gcc link]: https://gcc.gnu.org/
[Intel link]: https://software.intel.com/en-us/fortran-compilers
[CMake link]: https://cmake.org
