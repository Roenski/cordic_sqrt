import math


def cordic_sqrt_hwish(mantissa, exponent):
    # For double precision:
    # Mantissa: 53 bits (including sign bit, excluding hidden bit)
    # Exponent: 11 bits
    # Exponent bias: 2^10-1 = 1023
    # Mantissa is a value between 1-2 (including hidden bit)
    # However, if exponent is odd, mantissa must be divided by 2 and exponent added by one

    bias = 127

    exponent_unbiased = exponent - bias

    if exponent_unbiased == 0:
        new_exponent = exponent_unbiased
        mantissa_start = mantissa
    else:
        # exponent is odd
        if exponent_unbiased % 2:
            new_exponent = (exponent_unbiased+1)/2
            mantissa_start = mantissa / 2
        # exponent is even
        else:
            new_exponent = exponent_unbiased/2
            mantissa_start = mantissa

    mantissa_sqrt, _, _ = cordic_sqrt_ideal(mantissa_start)

    if mantissa_sqrt < 1:
        new_exponent -= 1
        mantissa_sqrt *= 2

    return (mantissa_sqrt, new_exponent+bias)





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
    #print(cordic_sqrt_ideal(1.5))
    print(cordic_sqrt_hwish(1.2799999713897705, 120))

