options(digits=4)
View(analysis_URXOP1_crt$variables |> group_by(SDDSRVYR) |> summarise(LLOD=min(URXOP1), Number_LLOD=sum(URXOP1==min(URXOP1)), n=n(), Percent_above_LLOD=round((100*(1-(Number_LLOD/n))), 4)))


View(analysis_URXOP2_crt$variables |> group_by(SDDSRVYR) |> summarise(LLOD=min(URXOP2), Number_LLOD=sum(URXOP2==min(URXOP2)), n=n(), Percent_above_LLOD=100*(1-(Number_LLOD/n))))


analysis_URXOP3_crt$variables |> group_by(SDDSRVYR) |> summarise(LLOD=min(URXOP3), Number_LLOD=sum(URXOP3==min(URXOP3)), n=n(), Percent_above_LLOD=100*(1-(Number_LLOD/n)))


analysis_URXOP4_crt$variables |> group_by(SDDSRVYR) |> summarise(LLOD=min(URXOP4), Number_LLOD=sum(URXOP4==min(URXOP4)), n=n(), Percent_above_LLOD=1-(Number_LLOD/n))


analysis_URXOP5_crt$variables |> group_by(SDDSRVYR) |> summarise(LLOD=min(URXOP5), Number_LLOD=sum(URXOP5==min(URXOP5)), n=n(), Percent_above_LLOD=1-(Number_LLOD/n))

analysis_URXOP6_crt$variables |> group_by(SDDSRVYR) |> summarise(LLOD=min(URXOP6), Number_LLOD=sum(URXOP6==min(URXOP6)), n=n(), Percent_above_LLOD=1-(Number_LLOD/n))
