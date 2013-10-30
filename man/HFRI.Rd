\name{HFRI Constituent Returns}
\alias{HFRI}
\docType{data}
\title{
Hedge Fund Index Returns
}
\description{
Monthly performance on three hedge funds from the HFRI index in addition to a composite HFRI return.  Includes monthly returns (where available) from January 1996 through June 2013.
} 
\usage{data(HFRI)}
\format{
  A data frame with 210 observations on the following 5 variables.
  \describe{
    \item{\code{date}}{Last trading date in month}
    \item{\code{EHQD}}{Monthly return on an Equity Hedge Quantitative Directional fund}
    \item{\code{EDMA}}{Monthly return on an Event Driven Merger Arbitrage fund}
    \item{\code{EDDR}}{Monthly return on an Event Driven Distressed/Restructuring fund}
    \item{\code{HFRI}}{Monthly return of the HFRI Fund Weighted Composite Index}
  }
}
\details{
For return variables .01 = 1\%.

Hedge Fund Research, Inc. (HFR) is the global leader in the alternative investment industry. Established in 1992, HFR specializes in the areas of indexation and analysis of hedge funds.  The HFRI Fund Weighted Composite Index is the industry's most widely used standard benchmark of hedge fund performance globally.

The three funds selected are classified using HFR's methodology into either "Equity Hedge" or "Event Driven", and then more specifically into "Quantitative Directional", "Merger Arbitrage", or "Distressed/Restructuring".

\emph{Equity Hedge}: Equity Hedge strategies maintain positions both long and short in primarily equity and equity derivative securities. A wide variety of investment processes can be employed to arrive at an investment decision, including both quantitative and fundamental techniques; strategies can be broadly diversified or narrowly focused on specific sectors and can range broadly in terms of levels of net exposure, leverage employed, holding period, concentrations of market capitalizations and valuation ranges of typical portfolios. Equity Hedge managers would typically maintain at least 50\% exposure to, and may in some cases be entirely invested in, equities - both long and short.

\emph{Event Driven}: Investment Managers who maintain positions in companies currently or prospectively involved in corporate transactions of a wide variety including but not limited to mergers, restructurings, financial distress, tender offers, shareholder buybacks, debt exchanges, security issuance or other capital structure adjustments. Security types can range from most senior in the capital structure to most junior or subordinated, and frequently involve additional derivative securities. Event Driven exposure includes a combination of sensitivities to equity markets, credit markets and idiosyncratic, company specific developments. Investment theses are typically predicated on fundamental characteristics (as opposed to quantitative), with the realization of the thesis predicated on a specific development exogenous to the existing capital structure.

\emph{Quantitative Directional} strategies employ sophisticated quantitative techniques of analyzing price data to ascertain information about future price movement and relationships between securities, select securities for purchase and sale. These can include both Factor-based and Statistical Arbitrage/Trading strategies. Factor-based investment strategies include strategies in which the investment thesis is predicated on the systematic analysis of common relationships between securities. Statistical Arbitrage/Trading strategies consist of strategies in which the investment thesis is predicated on exploiting pricing anomalies which may occur as a function of expected mean reversion inherent in security prices; high frequency techniques may be employed and trading strategies may also be employed on the basis on technical analysis or opportunistically to exploit new information the investment manager believes has not been fully, completely or accurately discounted into current security prices. Quantitative Directional Strategies typically maintain varying levels of net long or short equity market exposure over various market cycles.

\emph{Merger Arbitrage} strategies which employ an investment process primarily focused on opportunities in equity and equity related instruments of companies which are currently engaged in a corporate transaction. Merger Arbitrage involves primarily announced transactions, typically with limited or no exposure to situations which pre-, post-date or situations in which no formal announcement is expected to occur. Opportunities are frequently presented in cross border, collared and international transactions which incorporate multiple geographic regulatory institutions, with typically involve minimal exposure to corporate credits. Merger Arbitrage strategies typically have over 75\% of positions in announced transactions over a given market cycle.

\emph{Distressed Restructuring} strategies employ an investment process focused on corporate fixed income instruments, primarily on corporate credit instruments of companies trading at significant discounts to their value at issuance or obliged (par value) at maturity as a result of either formal bankruptcy proceeding or financial market perception of near term proceedings. Managers are typically actively involved with the management of these companies, frequently involved on creditors' committees in negotiating the exchange of securities for alternative obligations, either swaps of debt, equity or hybrid securities. Managers employ fundamental credit processes focused on valuation and asset coverage of securities of distressed firms; in most cases portfolio exposures are concentrated in instruments which are publicly traded, in some cases actively and in others under reduced liquidity but in general for which a reasonable public market exists. In contrast to Special Situations, Distressed Strategies employ primarily debt (greater than 60\%) but also may maintain related equity exposure.

The HFRI Fund Weighted Composite Index contains over 2,200 constituent funds.  It is an equal weighted index and includes both domestic and offshore funds.  The constituent funds have at least $50 Million under management or have been actively trading for at least twelve months.


}

\source{
\emph{Hedge Fund Research Database}

\emph{https://www.hedgefundresearch.com/}
}
\examples{
#Merge HFRI data with with risk free returns (inner join)
data(marketRf)
data(HFRI)
HFRIRf<-merge(HFRI,marketRf,by="date")
}
\keyword{datasets}
