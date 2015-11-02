.PHONY: dialyze

dialyze: otp_18.1.plt
	dialyzer --plt otp_18.1.plt -r ebin --statistics -Wunmatched_returns -Werror_handling -Wunknown
    
    
otp_18.1.plt:
	wget http://download.erlide.org/tools/otp_18.1.plt
    
    