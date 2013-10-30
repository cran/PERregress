\name{Vanguard Mutual Fund Returns}
\alias{Vanguard}
\docType{data}
\title{
Vanguard Mutual Fund Return Data
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
Historical monthly returns from the following nine Vanguard mutual funds:\cr

- Vanguard Total Stock Market Index Fund Admiral Shares (VTSAX)\cr
- Vanguard Mid-Cap Growth Fund (VMGRX)\cr
- Vanguard Equity Income Fund Investor Shares (VEIPX)\cr
- Vanguard 500 Index Fund Admiral Shares (VFIAX)\cr
- Vanguard Windsor II Fund Investor Shares (VWNFX) \cr
- Vanguard Small-Cap Index Fund Admiral Shares (VSMAX)\cr
- Vanguard Growth and Income Fund Investor Shares (VQNPX)\cr
- Vanguard Energy Fund Investor Shares (VGENX)\cr
- Vanguard Health Care Fund Investor Shares (VGHCX)\cr

Includes all available monthly returns from June 1984 through June 2013.

}
\usage{data(Vanguard)}
\format{
  A data frame with 2294 observations on the following 5 variables.
  \describe{
    \item{\code{date}}{Last trading date in month}
    \item{\code{ticker}}{Ticker symbol}
    \item{\code{crsp_fundno}}{CRSP fund number}
    \item{\code{mtna}}{Total net asset value}
    \item{\code{mret}}{Monthly return with .01 = 1\%}
  }
}
\details{
Fund descriptions from Vanguard:

Vanguard Total Stock Market Index Fund Admiral Shares (VTSAX) is designed to provide investors with exposure to the entire U.S. equity market, including small-, mid-, and large-cap growth and value stocks. The fund's key attributes are its low costs, broad diversification, and the potential for tax efficiency. Investors looking for a low-cost way to gain broad exposure to the U.S. stock market who are willing to accept the volatility that comes with stock market investing may wish to consider this fund as either a core equity holding or your only domestic stock fund.

Vanguard Mid-Cap Growth Fund (VMGRX) is an actively managed, mid-capitalization fund and invests primarily in the stocks of mid-size domestic companies that the fund's investment managers believe have stronger earnings and revenue growth prospects than the average mid-cap company. Investors who are seeking exposure to the mid-cap arena of the U.S. stock market and who are willing to endure the volatility that can come from an investment in stocks may wish to consider this fund as an option for their portfolio.

Vanguard Equity Income Fund Investor Shares (VEIPX) is designed to provide investors with an above-average level of current income while offering exposure to the stock market. Since the fund typically invests in companies that are dedicated to consistently paying dividends, it may have a higher yield than other Vanguard stock mutual funds. The fund's emphasis on slower-growing, higher-yielding companies can also mean that its total return may not be as strong in a significant bull market. This income-focused fund may be appropriate for investors who have a long-term investment goal and a tolerance for stock market volatility.

Vanguard 500 Index Fund Admiral Shares (VFIAX) is a low cost way to gain diversified exposure to the U.S. equity market. The fund invests in 500 of the largest U.S. companies, which span many different industries and account for about three-fourths of the U.S. stock market's value. The key risk for the fund is the volatility that comes with its full exposure to the stock market. Because the 500 Index Fund is broadly diversified within the large-capitalization market, it may be considered a core equity holding in a portfolio.

Vanguard Windsor II Fund Investor Shares (VWNFX) carries the same risk associated with the stock market, but this "value" conscious approach may provide a less bumpy ride. That said, the fund may not keep up in a strong bull market. If you have a long-term investment goal and want less market volatility than might be present in a more aggressive investment, the fund could be a good fit for you.

Vanguard Small-Cap Index Fund Admiral Shares (VSMAX) is a low-cost index fund that provides broad exposure to the small-capitalization U.S. equity market. The fund seeks to track an index of small-sized companies. One of the fund's primary risks is its focus on the small-cap arena, which is an often-volatile segment of the market. Investors looking to add a small-cap equity allocation to an already diversified portfolio may wish to consider this option.

Vanguard Growth and Income Fund Investor Shares (VQNPX) seeks to outperform the Standard & Poor's 500 Index. The fund's quantitative investment approach results in risk and sector profiles that are similar to those of its benchmark index. The fund has a total return goal, meaning that it seeks both capital appreciation and dividend income. The key risk for the fund is the volatility that comes with its full exposure to the stock market. The fund provides broad exposure to a diverse group of large U.S. companies and may be considered a primary equity holding in a portfolio.

Vanguard Energy Fund Investor Shares (VGENX) offers investors low-cost exposure to U.S. and non-U.S. companies that are engaged in various aspects of the energy business. Unlike similar funds, which may invest in companies that explore for and distribute additional commodities, this fund focuses purely on energy, such as oil, natural gas, and coal. The fund is narrow in scope, investing solely within the energy sector. In addition, the fund may be invested up to 100\% in foreign securities, which can be more volatile than domestic holdings. Returns may vary widely from year-to-year, so this fund may be considered complementary to an already diversified portfolio with a long-term time horizon.

Vanguard Health Care Fund Investor Shares (VGHCX) offers investors low-cost exposure to domestic and foreign companies involved in various aspects of the health care industry, such as pharmaceutical firms, medical supply companies, and research firms. The fund tends to be more geographically diverse and exhibit lower turnover than other health care funds. Still, one risk to note is the fund's narrow scope, investing solely within the health care sector. Returns may vary widely from year to year, so this fund should be considered complementary to an already diversified portfolio with a long-term time horizon.

For additional information on specific funds, visit https://investor.vanguard.com/home/
}
\source{
\emph{Center for Research in Security Prices}
}
\examples{
#Merge Vanguard returns with risk free returns
data(marketRf)
data(Vanguard)
VanguardRf<-merge(Vanguard,marketRf,by="date",all.x=TRUE)

#Plot monthly returns of VMGRX versus 1-Month T-Bill returns
plot(mret~RF,data=VanguardRf[Vanguard$ticker=="VMGRX",])

#Obtain average monthly return of each mutual fund
avgRet<-aggregate(mret~ticker,data=Vanguard,FUN=mean)
}
\keyword{datasets}
