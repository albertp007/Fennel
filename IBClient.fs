namespace QuantFin

module IBClient =

  open IBApi

  /// <summary>Dummy implementation of the EWrapper interface </summary>
  ///
  type EWrapperImpl() = 
    interface EWrapper with

      member this.accountDownloadEnd(account: string) =
        printfn "%s" account

      member this.accountSummary(reqId, account, tag, value, currency) =
        printfn "accountSummary(%d)" reqId

      member this.accountSummaryEnd(reqId) =
        printfn "accountSummaryEnd %d" reqId

      member this.bondContractDetails(reqId, contract) =
        printfn "bondContractDetails(%d) %A" reqId contract

      member this.commissionReport(commissionReport) =
        printfn "commissionReport %A" commissionReport

      member this.connectionClosed() =
        printfn "connectionClosed"

      member this.contractDetails(reqId, contractDetails) =
        printfn "contractDetails(%d) %A" reqId contractDetails

      member this.contractDetailsEnd(reqId) =
        printfn "contractDetailsEnd(%d)" reqId

      member this.currentTime(time) =
        printfn "currentTime %d" time

      member this.deltaNeutralValidation(reqId, underComp) =
        printfn "deltaNeutralValidation(%d) %A" reqId underComp

      member this.displayGroupList(reqId, group) =
        printfn "displayGroup(%d) %s" reqId group

      member this.displayGroupUpdated(reqId, group) =
        printfn "displayGroupUpdated(%d) %s" reqId group

      member this.error(id, errorCode, errMsg) =
        printfn "error %d %d %s" id errorCode errMsg

      member this.error(msg) =
        printfn "error %s" msg

      member this.error(e: exn) =
        printfn "error %s" e.Message

      member this.execDetails(reqId, contract, execution) =
        printfn "%d %A %A" reqId contract execution

      member this.execDetailsEnd(reqId) =
        printfn "execDetailsEnd(%d)" reqId

      member this.fundamentalData(reqId, data) =
        printfn "fundamental data(%d) %s" reqId data

      member this.historicalData(reqId, date, o, h, l, c, volume, count,
                                  vwap, hasGaps) =
        printfn "historicalData(%d) %s %f %f %f %f %d %d %f %b" reqId date o h
              l c volume count vwap hasGaps

      member this.historicalDataEnd(reqId, s, e) =
        printfn "historicalDataEnd(%d) %s %s" reqId s e

      member this.managedAccounts(accountsList) =
        printfn "%s" accountsList

      member this.marketDataType(reqId, marketDataType) =
        printfn "marketData(%d) %d" reqId marketDataType

      member this.nextValidId(orderId) =
        printfn "nextValidId %d" orderId

      member this.openOrder(orderId, contract, order, orderState) =
        printfn "openOrder %d %A %A %A" orderId contract order orderState

      member this.openOrderEnd() =
        printfn "openOrderEnd"

      member this.orderStatus(orderId, status, filled, remaining, avgPrice,
                              permId, parentId, lastPrice, clientId,
                              whyHeld) =
        printfn "orderStatus %d %s %d %d %f %d %d %f %d %s" orderId status
          filled remaining avgPrice permId parentId lastPrice clientId whyHeld

      member this.position(account, contract, pos, avgCost) =
        printfn "position %s %A %d %f" account contract pos avgCost

      member this.positionEnd() =
        printfn "positionEnd"

      member this.realtimeBar(reqId, time, o, h, l, c, v, vwap, count) =
        printfn "realtimeBar(%d) %d %f %f %f %f %d %f %d" reqId time
          o h l c v vwap count

      member this.receiveFA(faDataType, faXmlData) =
        printfn "receiveFA %d %s" faDataType faXmlData

      member this.scannerData(reqId, rank, contractDetails, distance,
                              benchmark, projection, legStr) =
        printfn "scannerData(%d) %d %A %s %s %s %s" reqId rank contractDetails
          distance benchmark projection legStr

      member this.scannerDataEnd(reqId) =
        printfn "scannerDataEnd(%d)" reqId

      member this.scannerParameters(xml) =
        printfn "scannerParameters %s" xml

      member this.tickEFP(tickerId, tickType, basisPoints, formattedBasisPoints,
                          impliedFuture, holdDays, futureExpiry, dividendImpact,
                          dividendsToExpiry) =
        printfn "tickEEP %d %d %f %s %f %d %s %f %f" tickerId tickType
          basisPoints formattedBasisPoints impliedFuture holdDays futureExpiry
          dividendImpact dividendsToExpiry

      member this.tickGeneric(tickerId, field, value) =
        printfn "tickGeneric %d %d %f" tickerId field value

      member this.tickOptionComputation(tickerId, field, impliedVolatility,
                                        delta, optPrice, pvDividend, gamma,
                                        vega, theta, undPrice) =
        printfn "tickOptionComputation %d %d %f %f %f %f %f %f %f %f"
          tickerId field impliedVolatility delta optPrice pvDividend gamma
          vega theta undPrice

      member this.tickPrice(tickerId, field, price, canAutoExecute) =
        printfn "tickPrice %d %d %f %d" tickerId field price canAutoExecute

      member this.tickSize(tickerId, field, size) =
        printfn "tickSize %d %d %d" tickerId field size

      member this.tickSnapshotEnd(tickerId) =
        printfn "tickSnapshotEnd %d" tickerId

      member this.tickString(tickerId, field, value) =
        printfn "tickString %d %d %s" tickerId field value

      member this.updateAccountTime(timestamp) =
        printfn "updateAccountTime %s" timestamp

      member this.updateAccountValue(key, value, currency, accountName) =
        printfn "updateAccountValue %s %s %s %s" key value currency accountName

      member this.updateMktDepth(tickerId, position, operation, side, price,
                                  size) =
        printfn "updateMktDepth %d %d %d %d %f %d" tickerId position operation
          side price size

      member this.updateMktDepthL2(tickerId, position, marketMaker, operation,
                                   side, price, size) =
        printfn "updateMktDepthL2 %d %d %s %d %d %f %d" tickerId position
          marketMaker operation side price size

      member this.updateNewsBulletin(msgId, msgType, msg, origExchange) =
        printfn "updateNewsBulletin %d %d %s %s" msgId msgType msg origExchange

      member this.updatePortfolio(contract, position, marketPrice, marketValue,
                                  averageCost, unrealizedPNL, realizedPNL,
                                  accountName) =
        printfn "updatePortfolio %A %d %f %f %f %f %f %s" contract position
          marketPrice marketValue averageCost unrealizedPNL realizedPNL
          accountName

      member this.verifyCompleted(isSuccessful, errorText) =
        printfn "verifyCompleted %b %s" isSuccessful errorText

      member this.verifyMessageAPI(apiData) =
        printfn "verifyMessageAPI %s" apiData

  /// <summary>Test function for testing connection and request current time
  /// </summary>
  ///
  let testConnect() =
    let wrapper = EWrapperImpl()
    let client = EClientSocket(wrapper)
    client.eConnect("127.0.0.1", 4001, 0, false)
    client.reqCurrentTime()
    System.Threading.Thread.Sleep(2000)
    client.eDisconnect()

  /// <summary>Test suites</summary>
  ///
  let runTests() =
    testConnect()