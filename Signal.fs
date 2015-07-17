namespace QuantFin

module Signal =

    /// <summary>This function finds the occurrence of price patterns as defined
    /// by the function f from the list of price data</summary>
    /// <param name="f">the function which takes a list of price bars and
    /// returns an optional pair of (System.DataTime*System.DateTime) to
    /// indicate the date time of the start of the pattern and the end of the
    /// pattern</param>
    /// <param name="prices">the list of price bars in which patterns are to be
    /// found</param>
    /// <returns>list of (System.DateTime*System.DateTime) pairs indicating the
    /// start and end date of all occurrences of the pattern</returns>
    let rec findPattern f prices =
        let rec findPatternHelp f acc prices =
            match prices with
            | [] -> acc
            | h::t ->
                match f prices with
                | Some startEndDate -> findPatternHelp f (startEndDate::acc) t
                | None -> findPatternHelp f acc t
        findPatternHelp f [] prices

    /// <summary>This function defines the bearish engulfing candlestick pattern
    /// </summary>
    /// <param name="prices">list of price bars to process</param>
    /// <returns>optional pair of System.DateTime indicating the start time and
    /// end time of the pattern if found</returns>
    let bearishEngulf (prices: QuantFin.Data.Bar list) =
        match prices with
        | h1::h2::_ -> if h2.Open > h1.Close && h2.Close < h1.Open &&
                        h1.Open < h1.Close then
                        Some (h1.Date, h2.Date) else None
        | _ -> None

    /// <summary>This function calculates the simple moving average of a list of
    /// float values</summary>
    /// <param name="n">Size of the moving window</param>
    /// <param name="lst">The list of float values</param>
    /// <returns>The first n-1 values will be 0. The values onwards are the
    /// moving average calculated at the time point n</returns>
    ///
    let movingAverage n lst =
        //
        // Helper function
        // i - keep track of the first n iterations, when the queue is not full
        // q - a queue to keep track of the last n elements
        // total - current total of all the elements in q
        // acc - the result
        //
        let rec movingAverage' n lst i q total acc =
            match lst with
            | [] -> List.rev acc
            | h::t ->
                if i < n then
                    //
                    // total' is the new total with a new element from the list
                    // since i < n, the queue is not yet full and therefore we
                    // simply add h to total
                    //
                    let total' = total + h
                    let tmp = if i = n - 1 then total'/(float n) else 0.0
                    movingAverage' n t (i+1) (h::q) total' (tmp::acc)
                else
                    //
                    // the queue is already full, therefore we reverse the queue
                    // in order to pop the oldest element in the queue and push
                    // the newest element onto it by cons-ing h with the reverse
                    // of the tail part qt
                    let qh::qt = List.rev q

                    // the new total is the old total - the oldest element + h
                    let total' = total - qh + h
                    let avg = total'/(float n)
                    movingAverage' n t n (h::(List.rev qt)) total' (avg::acc)
        movingAverage' n lst 0 [] 0.0 []

    /// <summary>This function calculates the dot product of two lists of
    /// float numbers </summary>
    /// <param name="v1">the first vector of floats</param>
    /// <param name="v2">the second vector of floats</param>
    /// <returns>The dot product of v1 and v2</returns>
    ///
    let dot v1 v2 =
        List.fold (fun a (x, y)->a + x*y) 0.0 (List.zip v1 v2)

    /// <summary>This function is a simplified version of the filter() function
    /// in R. </summary>
    /// <param name="w">Vector of weights (float)</param>
    /// <param name="lst">The list of float values</param>
    /// <returns>The series of numbers w0x0+w1x1+...+wnxn,
    /// w0x1+w1x2+...+wnx(n+1) ...</returns>
    ///
    let filter w lst =
        //
        // Helper function
        // i - keep track of the first n iterations, when the queue is not full
        // q - a queue to keep track of the last n elements
        // total - current total of all the elements in q
        // acc - the result
        //
        let m = List.length w
        let w' = List.rev w
        let rec filter' n w lst i q acc =
            match lst with
            | [] -> List.rev acc
            | h::t ->
                    // q isn't full yet, keeping pushing
                    let q' = if i < n then (h::q) else
                        // q is full, pop at the end and push in the front
                        h::(List.rev (List.tail (List.rev q)))
                    let tmp = if i >= n - 1 then (dot w q') else 0.0
                    let n' = if i < n then i+1 else n
                    filter' n w t n' q' (tmp::acc)
        filter' m w' lst 0 [] []
