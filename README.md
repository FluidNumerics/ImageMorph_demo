# ImageMorph_demo
Small Fortran program for performing stencil operations on 2-D RGB data.
This source code is used as part of exercises for porting Fortran applications to graphics processing units (GPUs)

# Getting Started
   ## Clone the repo
       git clone git@github.com:FluidNumerics/ImageMorph_demo.git

   ## Build the executable
   Change directories so that you are in the main directory
   
       cd ImageMorph_demo
       
   Check the `environment_settings` file to make sure that the Fortran compiler `FC` is set appropriately. Once it is set, source the environment settings file and run make
   
       source environment_settings
       make
       
   
   ## Run the application
       ./MorphImage
       
The main source code can be found under the directory
        rgbso/src/MorphImage.f90 
