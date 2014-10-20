// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef __ggcomp_RcppExports_h__
#define __ggcomp_RcppExports_h__

#include <Rcpp.h>

namespace ggcomp {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("ggcomp", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("ggcomp", "ggcomp_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in ggcomp");
            }
        }
    }

    inline List condense_count(const NumericVector& x, double origin, double width, bool pad, bool right_closed, const NumericVector& w) {
        typedef SEXP(*Ptr_condense_count)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_condense_count p_condense_count = NULL;
        if (p_condense_count == NULL) {
            validateSignature("List(*condense_count)(const NumericVector&,double,double,bool,bool,const NumericVector&)");
            p_condense_count = (Ptr_condense_count)R_GetCCallable("ggcomp", "ggcomp_condense_count");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_condense_count(Rcpp::wrap(x), Rcpp::wrap(origin), Rcpp::wrap(width), Rcpp::wrap(pad), Rcpp::wrap(right_closed), Rcpp::wrap(w));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<List >(__result);
    }

    inline List condense_sum(const NumericVector& x, double origin, double width, bool pad, bool right_closed, const NumericVector& z, const NumericVector& w) {
        typedef SEXP(*Ptr_condense_sum)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_condense_sum p_condense_sum = NULL;
        if (p_condense_sum == NULL) {
            validateSignature("List(*condense_sum)(const NumericVector&,double,double,bool,bool,const NumericVector&,const NumericVector&)");
            p_condense_sum = (Ptr_condense_sum)R_GetCCallable("ggcomp", "ggcomp_condense_sum");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_condense_sum(Rcpp::wrap(x), Rcpp::wrap(origin), Rcpp::wrap(width), Rcpp::wrap(pad), Rcpp::wrap(right_closed), Rcpp::wrap(z), Rcpp::wrap(w));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<List >(__result);
    }

    inline List condense_moments(const NumericVector& x, double origin, double width, bool pad, bool right_closed, const NumericVector& z, const NumericVector& w, int moments) {
        typedef SEXP(*Ptr_condense_moments)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_condense_moments p_condense_moments = NULL;
        if (p_condense_moments == NULL) {
            validateSignature("List(*condense_moments)(const NumericVector&,double,double,bool,bool,const NumericVector&,const NumericVector&,int)");
            p_condense_moments = (Ptr_condense_moments)R_GetCCallable("ggcomp", "ggcomp_condense_moments");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_condense_moments(Rcpp::wrap(x), Rcpp::wrap(origin), Rcpp::wrap(width), Rcpp::wrap(pad), Rcpp::wrap(right_closed), Rcpp::wrap(z), Rcpp::wrap(w), Rcpp::wrap(moments));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<List >(__result);
    }

    inline List condense_median(const NumericVector& x, double origin, double width, bool pad, bool right_closed, const NumericVector& z, const NumericVector& w) {
        typedef SEXP(*Ptr_condense_median)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_condense_median p_condense_median = NULL;
        if (p_condense_median == NULL) {
            validateSignature("List(*condense_median)(const NumericVector&,double,double,bool,bool,const NumericVector&,const NumericVector&)");
            p_condense_median = (Ptr_condense_median)R_GetCCallable("ggcomp", "ggcomp_condense_median");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_condense_median(Rcpp::wrap(x), Rcpp::wrap(origin), Rcpp::wrap(width), Rcpp::wrap(pad), Rcpp::wrap(right_closed), Rcpp::wrap(z), Rcpp::wrap(w));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<List >(__result);
    }

    inline NumericVector smooth_linear(const NumericVector& x_in, const NumericVector& z_in, const NumericVector& w_in, const NumericVector& x_out, const double h) {
        typedef SEXP(*Ptr_smooth_linear)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_smooth_linear p_smooth_linear = NULL;
        if (p_smooth_linear == NULL) {
            validateSignature("NumericVector(*smooth_linear)(const NumericVector&,const NumericVector&,const NumericVector&,const NumericVector&,const double)");
            p_smooth_linear = (Ptr_smooth_linear)R_GetCCallable("ggcomp", "ggcomp_smooth_linear");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_smooth_linear(Rcpp::wrap(x_in), Rcpp::wrap(z_in), Rcpp::wrap(w_in), Rcpp::wrap(x_out), Rcpp::wrap(h));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<NumericVector >(__result);
    }

    inline NumericVector smooth_robust(const NumericVector& x_in, const NumericVector& z_in, const NumericVector& w_in, const NumericVector& x_out, const double h, int iterations = 3) {
        typedef SEXP(*Ptr_smooth_robust)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_smooth_robust p_smooth_robust = NULL;
        if (p_smooth_robust == NULL) {
            validateSignature("NumericVector(*smooth_robust)(const NumericVector&,const NumericVector&,const NumericVector&,const NumericVector&,const double,int)");
            p_smooth_robust = (Ptr_smooth_robust)R_GetCCallable("ggcomp", "ggcomp_smooth_robust");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_smooth_robust(Rcpp::wrap(x_in), Rcpp::wrap(z_in), Rcpp::wrap(w_in), Rcpp::wrap(x_out), Rcpp::wrap(h), Rcpp::wrap(iterations));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<NumericVector >(__result);
    }

    inline NumericVector smooth_mean(const NumericVector& x_in, const NumericVector& z_in, const NumericVector& w_in, const NumericVector& x_out, const double h) {
        typedef SEXP(*Ptr_smooth_mean)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_smooth_mean p_smooth_mean = NULL;
        if (p_smooth_mean == NULL) {
            validateSignature("NumericVector(*smooth_mean)(const NumericVector&,const NumericVector&,const NumericVector&,const NumericVector&,const double)");
            p_smooth_mean = (Ptr_smooth_mean)R_GetCCallable("ggcomp", "ggcomp_smooth_mean");
        }
        RObject __result;
        {
            RNGScope __rngScope;
            __result = p_smooth_mean(Rcpp::wrap(x_in), Rcpp::wrap(z_in), Rcpp::wrap(w_in), Rcpp::wrap(x_out), Rcpp::wrap(h));
        }
        if (__result.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (__result.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(__result).c_str());
        return Rcpp::as<NumericVector >(__result);
    }

}

#endif // __ggcomp_RcppExports_h__
