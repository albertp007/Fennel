namespace QuantFin

module IBClient =
    
  open IBApi

  let EWrapperImpl = {
    new EWrapper with
      member this.accountDownloadEnd(account: string) =
        printf "%s" account

      member this.accountSummary(reqId: int, account: string, tag: string, 
                                 value: string, currency: string) =
        printf "accountSummary(%d)" reqId

      member this.accountSummaryEnd(reqId: int) =
        printf "accountSummaryEnd %d" reqId

      member this.bondContractDetails(reqId: int, contract: ContractDetails) =
        printf "bondContractDetails(%d) %A" reqId contract

      member this.commissionReport(commissionReport: CommissionReport) =
        printf "commissionReport %A" commissionReport

      member this.connectionClosed() =
        printf "connectionClosed"

      member this.contractDetails(reqId: int, contractDetails: ContractDetails) =
        printf "contractDetails(%d) %A" reqId contractDetails

      member this.contractDetailsEnd(reqId:int) =
        printf "contractDetailsEnd(%d)" reqId
      
      member this.currentTime(time) = 
        printf "currentTime %d" time

      member this.deltaNeutralValidation(reqId: int, underComp: UnderComp) =
        printf "deltaNeutralValidation(%d) %A" reqId underComp

      member this.displayGroupList(reqId: int, group: string) =
        printf "displayGroup(%d) %s" reqId group

      member this.displayGroupUpdated(reqId: int, group: string) =
        printf "displayGroupUpdated(%d) %s" reqId group

      member this.error(id: int, errorCode: int, errMsg: string) =
        printf "error %d %d %s" id errorCode errMsg

      member this.error(msg: string) = 
        printf "error %s" msg

      member this.error(e: exn) =
        printf "error %s" e.Message

      member this.execDetails(reqId, contract, execution) =
        printf "%d %A %A" reqId contract execution

      member this.execDetailsEnd(reqId) = 
        printf "execDetailsEnd(%d)" reqId

      member this.fundamentalData(reqId: int, data: string) =
        printf "fundamental data(%d) %s" reqId data

      member this.historicalData(reqId, date, o, h, l, c, volume, count, 
                                 vwap, hasGaps) =
        printf "historicalData(%d) %s %f %f %f %f %d %d %f %b" reqId date o h 
          l c volume count vwap hasGaps

      member this.historicalDataEnd(reqId, s, e) = 
        printf "historicalDataEnd(%d) %s %s" reqId s e

      member this.managedAccounts(accountsList) = 
        printf "%s" accountsList

      member this.marketDataType(reqId, marketDataType) = 
        printf "marketData(%d) %d" reqId marketDataType

      member this.nextValidId(orderId) = 
        printf "nextValidId %d" orderId

      member this.openOrder(orderId, contract, order, orderState) = 
        printf "openOrder %d %A %A %A" orderId contract order orderState
      
      member this.openOrderEnd() = 
        printf "openOrderEnd"
      
      member this.orderStatus(orderId, status, filled, remaining, avgPrice,
                              permId, parentId, lastPrice, clientId,
                              whyHeld) =
        printf "orderStatus %d %s %d %d %f %d %d %f %d %s" orderId status
          filled remaining avgPrice permId parentId lastPrice clientId whyHeld

      member this.position(account, contract, pos, avgCost) =
        printf "position %s %A %d %f" account contract pos avgCost

      member this.positionEnd() = 
        printf "positionEnd"

      member this.realtimeBar(reqId, time, o, h, l, c, v, vwap, count) =
        printf "realtimeBar(%d) %d %f %f %f %f %d %f %d" reqId time
          o h l c v vwap count

      member this.receiveFA(faDataType, faXmlData) = 
        printf "receiveFA %d %s" faDataType faXmlData

      member this.scannerData(reqId, rank, contractDetails, distance,
                              benchmark, projection, legStr) =
        printf "scannerData(%d) %d %A %s %s %s %s" reqId rank contractDetails
          distance benchmark projection legStr

      member this.scannerDataEnd(reqId) = 
        printf "scannerDataEnd(%d)" reqId

      member this.scannerParameters(xml) = 
        printf "scannerParameters %s" xml

      member this.tickEFP(tickerId, tickType, basisPoints, formattedBasisPoints,
                          impliedFuture, holdDays, futureExpiry, dividendImpact,
                          dividendsToExpiry) =
        printf "tickEEP %d %d %f %s %f %d %s %f %f" tickerId tickType
          basisPoints formattedBasisPoints impliedFuture holdDays futureExpiry
          dividendImpact dividendsToExpiry

      member this.tickGeneric(tickerId, field, value) =
        printf "tickGeneric %d %d %f" tickerId field value

      member this.tickOptionComputation(tickerId, field, impliedVolatility,
                                        delta, optPrice, pvDividend, gamma,
                                        vega, theta, undPrice) =
        printf "tickOptionComputation %d %d %f %f %f %f %f %f %f %f"
          tickerId field impliedVolatility delta optPrice pvDividend gamma
          vega theta undPrice

      member this.tickPrice(tickerId, field, price, canAutoExecute) =
        printf "tickPrice %d %d %f %d" tickerId field price canAutoExecute

      member this.tickSize(tickerId, field, size) =
        printf "tickSize %d %d %d" tickerId field size

      member this.tickSnapshotEnd(tickerId) =
        printf "tickSnapshotEnd %d" tickerId

      member this.tickString(tickerId, field, value) =
        printf "tickString %d %d %s" tickerId field value

      member this.updateAccountTime(timestamp) =
        printf "updateAccountTime %s" timestamp

      member this.updateAccountValue(key, value, currency, accountName) =
        printf "updateAccountValue %s %s %s %s" key value currency accountName

      member this.updateMktDepth(tickerId, position, operation, side, price,
                                 size) =
        printf "updateMktDepth %d %d %d %d %f %d" tickerId position operation
          side price size

      member this.updateMktDepthL2(tickerId, position, marketMaker, operation,
                                   side, price, size) =
        printf "updateMktDepthL2 %d %d %s %d %d %f %d" tickerId position
          marketMaker operation side price size

      member this.updateNewsBulletin(msgId, msgType, msg, origExchange) =
        printf "updateNewsBulletin %d %d %s %s" msgId msgType msg origExchange

      member this.updatePortfolio(contract, position, marketPrice, marketValue,
                                  averageCost, unrealizedPNL, realizedPNL,
                                  accountName) =
        printf "updatePortfolio %A %d %f %f %f %f %f %s" contract position
          marketPrice marketValue averageCost unrealizedPNL realizedPNL
          accountName

      member this.verifyCompleted(isSuccessful, errorText) =
        printf "verifyCompleted %b %s" isSuccessful errorText

      member this.verifyMessageAPI(apiData) =
        printf "verifyMessageAPI %s" apiData
  }
  


 