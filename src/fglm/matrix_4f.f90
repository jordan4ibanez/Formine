module matrix_4f
  use, intrinsic :: iso_c_binding, only: c_float
  implicit none

  private

  !*
  !* As you can see, mat4f differs greatly from vec3f because it's
  !* only purpose is to do matrix math and shove that math into an
  !* OpenGL uniform.
  !*
  !* You can also probably see this differs greatly from regular fortran matrix
  !* math because it's using a flat array as a backing structure.
  !*
  !* This contains methods translated from JOML (Java) into Fortran.
  !*
  !* Please see joml.license for (MIT) licensing information.
  !*
  !* Why did I do this? I like JOML. That's about it!
  !*

  public :: mat4f

  !? This is an identifier to translate from JOML to Fortran. 00 based vs 1 linear based
  !? 1  2  3  4
  !? 5  6  7  8
  !? 9  10 11 12
  !? 13 14 15 16

  !?-------------
  !? | m00 | 1  |
  !? | m01 | 2  |
  !? | m02 | 3  |
  !? | m03 | 4  |
  !?-------------
  !? | m10 | 5  |
  !? | m11 | 6  |
  !? | m12 | 7  |
  !? | m13 | 8  |
  !?-------------
  !? | m20 | 9  |
  !? | m21 | 10 |
  !? | m22 | 11 |
  !? | m23 | 12 |
  !?-------------
  !? | m30 | 13 |
  !? | m31 | 14 |
  !? | m32 | 15 |
  !? | m33 | 16 |
  !?-------------



  type mat4f
    real(c_float), dimension(16) :: data = [ &
      1.0, 0.0, 0.0, 0.0, &
      0.0, 1.0, 0.0, 0.0, &
      0.0, 0.0, 1.0, 0.0, &
      0.0, 0.0, 0.0, 1.0 &
      ]
  contains
    !* There is only assignment operator. Everything else is too dangerous to implement.
    generic :: assignment(=) => assign_mat4f
    procedure :: assign_mat4f

    !* General methods.
    procedure :: identity
    procedure :: perspective

    !* Spacial methods.
    procedure :: rotate_x
    procedure :: rotate_y

    ! Set translation.
    procedure :: set_translation
    procedure :: set_translation_array
  end type mat4f

  interface mat4f
    module procedure :: constructor_scalar_f32, constructor_raw_f32, constructor_array_f32
  end interface mat4f

contains


  !* Constructor.


  type(mat4f) function constructor_scalar_f32(i) result(new_mat4f)
    implicit none
    real(c_float), intent(in), value :: i

    new_mat4f%data(1:16) = [i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i]
  end function constructor_scalar_f32


  type(mat4f) function constructor_raw_f32(x1,y1,z1,w1,x2,y2,z2,w2,x3,y3,z3,w3,x4,y4,z4,w4) result(new_mat4f)
    implicit none

    real(c_float), intent(in), value :: x1,y1,z1,w1,x2,y2,z2,w2,x3,y3,z3,w3,x4,y4,z4,w4

    new_mat4f%data(1:16) = [x1,y1,z1,w1,x2,y2,z2,w2,x3,y3,z3,w3,x4,y4,z4,w4]
  end function constructor_raw_f32


  type(mat4f) function constructor_array_f32(matrix_array) result(new_mat4f)
    implicit none

    real(c_float), dimension(16), intent(in) :: matrix_array

    new_mat4f%data(1:16) = matrix_array(1:16)
  end function constructor_array_f32


  !* Assignment.


  subroutine assign_mat4f(this, other)
    implicit none

    class(mat4f), intent(inout) :: this
    type(mat4f), intent(in), value :: other

    this%data(1:16) = other%data(1:16)
  end subroutine assign_mat4f


  !* General methods.


  subroutine identity(this)
    implicit none

    class(mat4f), intent(inout) :: this

    this%data(1:16) = [ &
      1.0, 0.0, 0.0, 0.0, &
      0.0, 1.0, 0.0, 0.0, &
      0.0, 0.0, 1.0, 0.0, &
      0.0, 0.0, 0.0, 1.0 &
      ]
  end subroutine identity


  !* Translated from JOML.
  subroutine perspective(this, fov_y_radians, aspect_ratio, z_near, z_far)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none

    class(mat4f), intent(inout) :: this
    ! Very poor accuracy, if you're copying this in the future, first of all: Hi, I hope you're doing well. Second of all: You should use c_double or real64.
    ! I'm just using this like this so I can upload it straight into the GPU. (I am very lazy)
    real(c_float), intent(in), value :: fov_y_radians, aspect_ratio, z_near, z_far
    real(c_float) :: height, epsil
    real(c_float), dimension(4) :: r, n
    logical :: far_infinite, near_infinite
    ! Cache.
    real(c_float), dimension(16) :: mat

    mat = this%data

    height = tan(fov_y_radians * 0.5)
    r(1) = 1.0 / (height * aspect_ratio)
    r(2) = 1.0 / height

    far_infinite = (z_far > 0.0) .and. (.not. ieee_is_finite(z_far))
    near_infinite = (z_near > 0.0) .and. (.not. ieee_is_finite(z_near))

    if (far_infinite) then
      epsil = 1000000.0
      r(3) = epsil - 1.0
      r(4) = (epsil - 2.0) * z_near
    else if (near_infinite) then
      epsil = 1000000.0
      r(3) = 1.0 - epsil
      r(4) = (2.0 - epsil) * z_far
    else
      r(3) = (z_far + z_near) / (z_near - z_far)
      r(4) = (z_far + z_far) * z_near / (z_near - z_far)
    end if

    n(1) = mat(9)  * r(3) - mat(13)
    n(2) = mat(10) * r(3) - mat(14)
    n(3) = mat(11) * r(3) - mat(15)
    n(4) = mat(12) * r(3) - mat(16)

    print*,"fixme: use compacting operator (1:4) * r(1)"
    this%data(1:16) = [&
      mat(1:4) * r(1), &
      mat(5:8) * r(2), &
      n(1:4), &
      mat(9:12) * r(4) &
      ]
  end subroutine perspective


  !* Spacial methods.


  !* Translated from JOML. This method was called "rotateXInternal"
  subroutine rotate_x(this, angle_radians)
    use :: math_helpers, only: cos_from_sin_f32, fma_f32
    implicit none

    class(mat4f), intent(inout) :: this
    real(c_float), intent(in), value :: angle_radians
    real(c_float), dimension(3) :: translation
    real(c_float) :: sine, cosine
    real(c_float), dimension(8) :: lm
    ! Cache.
    real(c_float), dimension(16) :: mat

    !* Implementation note:
    !* Unlike JOML we will assume that this matrix has already been translated.
    !* Worst case scenario: We are redundantly assigning 0.0 values.
    !* This keeps the implementation lean and simple.

    mat = this%data

    ! Save translation.
    translation = mat(13:15)

    sine = sin(angle_radians)

    cosine = cos_from_sin_f32(sine, angle_radians)

    lm = [mat(5:12)]

    this%data = [ &
      mat(1:4), &
    ! We break this up, because it becomes unwieldly.
      fma_f32(lm(1), cosine, lm(5) * sine), &
      fma_f32(lm(2), cosine, lm(6) * sine), &
      fma_f32(lm(3), cosine, lm(7) * sine), &
      fma_f32(lm(4), cosine, lm(8) * sine), &

      fma_f32(lm(1), -sine, lm(5) * cosine), &
      fma_f32(lm(2), -sine, lm(6) * cosine), &
      fma_f32(lm(3), -sine, lm(7) * cosine), &
      fma_f32(lm(4), -sine, lm(8) * cosine), &

      mat(13:16) &
      ]

    call this%set_translation_array(translation)
  end subroutine rotate_x

  subroutine rotate_y(this, angle_radians)
    use :: math_helpers, only: cos_from_sin_f32, fma_f32
    implicit none

    class(mat4f), intent(inout) :: this
    real(c_float), intent(in), value :: angle_radians

    real(c_float) :: sine, cosine

  end subroutine rotate_y

  subroutine set_translation(this, x,y,z)
    implicit none

    class(mat4f), intent(inout) :: this
    real(c_float), intent(in), value :: x,y,z

    this%data(13:15) = [x, y, z]
  end subroutine set_translation


  subroutine set_translation_array(this, xyz)
    implicit none

    class(mat4f), intent(inout) :: this
    real(c_float), dimension(3), intent(in) :: xyz

    this%data(13:15) = xyz(1:3)
  end subroutine set_translation_array



end module matrix_4f
