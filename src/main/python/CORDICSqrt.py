import math



def cordic_sqrt_ideal(data_in):

    ideal_ans = math.sqrt(data_in)
    n = 300
    An = 1
    k = 4
    for i in range(1,n+1):
        An *= math.sqrt(1-2**(-2*i))
        if i == k:
            An *= math.sqrt(1-2**(-2*i))
            k = 3*k + 1
    k = 4
    xprev = data_in + 0.25
    yprev = data_in - 0.25
    for i in range(1, n+1):
        xtemp = xprev/(2**i)
        ytemp = yprev/(2**i)
        if yprev < 0:
            xprev += ytemp
            yprev += xtemp
        else:
            xprev -= ytemp
            yprev -= xtemp

        if i == k:
            xtemp = xprev/(2**i)
            ytemp = yprev/(2**i)
            if yprev < 0:
                xprev += ytemp
                yprev += xtemp
            else:
                xprev -= ytemp
                yprev -= xtemp
            k = 3*k + 1
    return xprev/An, ideal_ans, An

if __name__ == "__main__":
    print(cordic_sqrt_ideal(1.5))

