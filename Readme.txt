Workload breakdown:
Lauren: Handled scalation coding; wrote scalation regression and other modeling methods.
Raj: Preliminary selection and analysis of datasets in R, wrote analysis in report
Elise: Wrote R implementation for selection methods to produce desired output and plots, contributed to report plots and analysis

Expected output in R and Scalation:
Here are all the expected methods as well as bonus methods written in my code, corresponding to the rubric:
MLR: Forward, Backward, Stepwise, Ridge, Lasso
QR: Forward, Backward, Stepwise, Ridge, Lasso
QXR: Forward, Backward, Stepwise, Ridge, Lasso
CR: Forward, Backward, Stepwise, Ridge, Lasso
CXR: Forward, Backward, Stepwise, Ridge, Lasso

R README:

All of the R code is contained in the R markdown file. The code chunks can be run in order, as the method implementations are first, followed by their calls in the analysis of each dataset. To successfully run the analysis, it is necessary to update the class paths for each dataset based on where the datasets are stored on your computer. The datasets have also been submitted for your convenience. 
Some methods take a very long time to run and thus have been commented out for testing convenience, but you can uncomment them if you wish to test them manually, but it may take about 15-20 minutes for each commented method to run.

Scalation README:

As requested, for the scalation methods, I have separated my code into methods for each separate function (ie. forward quadratic, backward quadratic, stepwise quadratic, etc)
My code is all placed within a package called example. The datasets are uploaded as csvs and should be places in the same folder as the scalation files in order to be succesffully imported.
The scalation files are named with the convention projectDataSet.scala and should be places and run within the same folder. 
In order to run the files, press the number that corresponds to the method you wish to test while in sbt run environment. Some methods take a very long time to run and thus have been commented out for testing convenience, but you can uncomment them if you wish to test them manually, but it may take about 15-20 minutes for each commented method to run. 

In order to run my program, a few additions and changes were made to the scalation modeling scalation files. I have included a list of adjustments. The new jar packages should be recompiled with these additions and should be present within the sbt environment where the scalaiton code is run. Additions below:



1. Fit.scala:
	In Fit.scala, the qofVector definition was adjusted to include aic values. Here is the new method:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Collect qof results for a model having say 'l' parameters and return them
     *  in a vector.  Adjust 'index_?' to customize Quality of Fit (QoF) measures.
     *  @param fit     the fit vector with regard to the training set
     *  @param cv_fit  the fit array of statistics for cross-validation (upon test sets)
     */
    def qofVector (fit: VectoD, cv_fit: Array [Statistic]): VectorD =
    {
        val cv = if (cv_fit == null) -0.0                      // cv not computed
                 else                cv_fit(index_rSq).mean    // mean for R^2 cv
        VectorD (100 * fit(index_rSq),                         // R^2 percentage
                 100 * fit(index_rSqBar),                      // R^2 Bar percentage
                 100 * cv,                                     // R^2 cv percentage
                 fit(index_aic))                                     // AIC ADDITION LINE (THIS IS ADDED CODE)
    } // qofVector

2. PredictorMat.scala

	In PredictorMat.scala, three methods were edited. BackwardsElimAll and ForwardSelectAll both had the rSq matrix increased from size 3 to size 4 in order to store aic value as well. Here are the new methods:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive variables to have
     *  in the model, returning the variables added and the new Quality of Fit (QoF)
     *  measures for all steps.
     *  @see `Fit` for index of QoF measures.
     *  @param index_q  index of Quality of Fit (QoF) to use for comparing quality
     *  @param cross    whether to include the cross-validation QoF measure
     */
    def forwardSelAll (index_q: Int = index_rSqBar, cross: Boolean = true): (Set [Int], MatriD) =
    {
        val rSq  = new MatrixD (x.dim2 - 1, 4)                           // THIS IS LINE CHANGE: R^2, R^2 Bar, R^2 cv, NEW ADDITION: AIC
        val cols = Set (0)                                               // start with x_0 in model

        breakable { for (l <- 0 until x.dim2 - 1) {
            val (j, mod_j) = forwardSel (cols)                           // add most predictive variable
            if (j == -1) break
            cols     += j                                                // add variable x_j
            val fit_j = mod_j.fit
            rSq(l)    = if (cross) Fit.qofVector (fit_j, mod_j.crossValidate ())   // use new model, mod_j, with cross
                        else       Fit.qofVector (fit_j, null)                     // use new model, mod_j, no cross
            if (DEBUG) {
                val k = cols.size - 1
                println (s"==> forwardSel: add (#$k) variable $j, qof = ${fit_j(index_q)}")
            } // if
        }} // breakable for
        println(s"This is the rSq column" + rSq)

        (cols, rSq.slice (0, cols.size-1))
    } // forwardSelAll



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward selection to find the most predictive variables to have
     *  in the model, returning the variables added and the new Quality of Fit (QoF)
     *  measures for all steps.
     *  @see `Fit` for index of QoF measures.
     *  @param index_q  index of Quality of Fit (QoF) to use for comparing quality
     *  @param first    first variable to consider for elimination
     *  @param cross    whether to include the cross-validation QoF measure
     */
    def backwardElimAll (index_q: Int = index_rSqBar, first: Int = 1, cross: Boolean = true): (Set [Int], MatriD) =
    {
        val rSq  = new MatrixD (x.dim2 - 1, 4)                           // THIS IS LINE CHANGE: R^2, R^2 Bar, R^2 cv, NEW ADDITION: AIC
        val cols = Set (Array.range (0, x.dim2) :_*)                     // start with all x_j in model

        breakable { for (l <- 1 until x.dim2 - 1) {
            val (j, mod_j) = backwardElim (cols, first)                  // remove most predictive variable
            if (j == -1) break
            cols     -= j                                                // remove variable x_j
            val fit_j = mod_j.fit
            rSq(l)    = if (cross) Fit.qofVector (fit_j, mod_j.crossValidate ())   // use new model, mod_j, with cross
                        else       Fit.qofVector (fit_j, null)                     // use new model, mod_j, no cross
            if (DEBUG) {
                println (s"<== backwardElimAll: remove (#$l) variable $j, qof = ${fit_j(index_q)}")
            } // if
        }} // breakable for


        (cols, reverse (rSq.slice (1, rSq.dim1)))
    } // backwardElimAll

//Finally, the new method, stepwiseSelAll was also added to predictorMat in order to perform stepwise selection

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform stepwise selection to find the most predictive variables to have
     *  in the model, returning the variables added and the new Quality of Fit (QoF)
     *  measures for all steps.
     *  @see `Fit` for index of QoF measures.
     *  @param index_q  index of Quality of Fit (QoF) to use for comparing quality
     *  @param first    first variable to consider for elimination
     *  @param cross    whether to include the cross-validation QoF measure
     */

def stepwiseSelAll (index_q: Int = index_rSqBar, first: Int = 1, cross: Boolean = true): (Set [Int], MatriD) =
    {

        //make a new rSquared matrix and set all the columns equal to null, as we will start with forward selection
        val rSq  = new MatrixD (x.dim2 - 1, 4)                           // R^2, R^2 Bar, R^2 cv, AIC
        val rSqb  = new MatrixD (x.dim2 - 1, 4)                           // R^2, R^2 Bar, R^2 cv, AIC


	//Start from null model and perform forward selection
        val colsf = Set (0)
        val colsb = Set (0)
        val (h, mod_h) = forwardSel (colsf)
        colsf += h
        val fit_h = mod_h.fit
            rSq(0)    = if (cross) Fit.qofVector (fit_h, mod_h.crossValidate ())   // use new model, mod_j, with cross
                        else       Fit.qofVector (fit_h, null)                     // use new model, mod_j, no cross

        rSqb(0)    = if (cross) Fit.qofVector (fit_h, mod_h.crossValidate ())   // use new model, mod_j, with cross
                        else       Fit.qofVector (fit_h, null)                     // use new model, mod_j, no cross


        if (DEBUG) {
                val k = colsf.size - 1
                println (s"==> forwardSel: add (#$k) variable $h, qof = ${fit_h(index_q)}")
            } // if
         colsb += h
	//Now, perform forward and backward selection, compare the models, and choose the model with the higher r adjusted, updating the model until there are no new additions that increase R squared adj

        breakable { for (l <- 1 until x.dim2 - 1) {
            val (j, mod_j) = forwardSel (colsf)                           // add most predictive variable
            val (k, mod_k) = backwardElim(colsb)
            val old = rSq(l - 1).range(1)
            if (j == -1) break
            if (k == -1) break
            colsf     += j                                                // add variable x_j
            colsb -= k
        val fit_j = mod_j.fit
        val fit_k = mod_k.fit
	//store model metrics for both backward and selection to save for return
            rSq(l)    = if (cross) Fit.qofVector (fit_j, mod_j.crossValidate ())   // use new model, mod_j, with cross
                        else       Fit.qofVector (fit_j, null)                     // use new model, mod_j, no cross

            rSqb(l)    = if (cross) Fit.qofVector (fit_k, mod_k.crossValidate ())   // use new model, mod_j, with cross
                        else       Fit.qofVector (fit_k, null)                     // use new model, mod_j, no cross
	// update model with optimal selection

            if (rSq(l).range(1) < rSqb(l).range(1)) {
                rSq(l) = rSqb(l)
                colsf -= j
                colsf -= k

            if (DEBUG) {
                val b = colsf.size - 1
                println (s"==> backwardElim: rem (#$b) variable $k, qof = ${fit_k(index_q)}")
            } // if
        } else {
        colsb += k
        colsb  += j
        rSqb(l) = rSq(l)
        if (DEBUG) {

                val k = colsf.size - 1
                println (s"==> forwardSel: add (#$k) variable $j, qof = ${fit_j(index_q)}")
            } // if

}
// if new update is not better than old one, break 
if (rSq(l).range(1) < old) break

        }} // breakable for
//return the cols and the rsquared values corresponding to those shifts
        (colsf, rSq.slice (0, colsf.size-1))
   } // stepwiseSelAll




