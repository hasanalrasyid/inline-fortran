
#include <gsl/gsl_errno.h>

#include <gsl/gsl_matrix.h>

#include <gsl/gsl_odeiv2.h>

int inline_c_0_31a6738e02530b20854a5ca8638293b1032edaf3() {
return ( GSL_SUCCESS );
}


int inline_c_1_e29528c49fb8de767d1cbb346719b7380780375c(int (* funIO_inline_c_0)(double t, const double y[], double dydt[], void * params), int dim_c_inline_c_1, double x0_inline_c_2, double xend_inline_c_3, double * fMut_inline_c_4) {

      gsl_odeiv2_system sys = {
        funIO_inline_c_0,
        // The ODE to solve, converted to function pointer using the `fun`
        // anti-quoter
        NULL,                   // We don't provide a Jacobian
        dim_c_inline_c_1,           // The dimension
        NULL                    // We don't need the parameter pointer
      };
      // Create the driver, using some sensible values for the stepping
      // function and the tolerances
      gsl_odeiv2_driver *d = gsl_odeiv2_driver_alloc_y_new (
        &sys, gsl_odeiv2_step_rk8pd, 1e-6, 1e-6, 0.0);
      // Finally, apply the driver.
      int status = gsl_odeiv2_driver_apply(
        d, &x0_inline_c_2, xend_inline_c_3, fMut_inline_c_4);
      // Free the driver
      gsl_odeiv2_driver_free(d);
      return status;
    
}


int inline_c_2_4ea59fe2bc7fe63451ae4b926934f4e726884548() {
returnfortran ( GSL_EMAXITER );
}


float inline_c_3_baaeea09f5450836ff2ab16636aca4972767baa5() {
returnfortran ( GSL_EMAXITER_FLOAT );
}


int inline_c_4_b4b4adc018e7fe003e77992771fc803668198b63() {
return ( GSL_ENOPROG );
}


int inline_c_5_31a6738e02530b20854a5ca8638293b1032edaf3() {
return ( GSL_SUCCESS );
}

