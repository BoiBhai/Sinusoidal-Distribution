# -*- coding: utf-8 -*-
"""
Created on Tue May 28 19:05:25 2024

@author: Debayan
"""

# -*- coding: utf-8 -*-
"""
Created on Fri May 24 03:06:29 2024

@author: Debayan
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy import integrate

def sinu_curve(x, s, k, flip):
    """
    Computes the sinusoidal curve value.
    Parameters:
        x (float): The input value in the range [0, 1].
        s (float): Shape parameter.
        k (float): Exponent parameter.
        flip (bool): Whether to flip the sinusoidal curve.
    Returns:
        float: The value of the sinusoidal curve at x.
    """
    if 0 <= x <= 1:
        if not flip:
            return np.sin(np.pi * x**s) ** k
        else:
            return np.sin(np.pi * (1 - x)**s) ** k
    else:
        return 0

def curve_inner(x, s, k, flip):
    """
    Computes the inner curve by normalizing x with alpha and delta.
    Parameters:
        x (float): The input value in the normalized range [0, 1].
        s (float): Shape parameter for sinu_curve.
        k (float): Exponent parameter for sinu_curve.
        flip (bool): Whether to flip the sinusoidal curve.
    Returns:
        float: The value of the inner curve at x.
    """
    return sinu_curve(x, s, k, flip)

def pdf(x, alpha=0, delta=1, s=1, k=1, flip=False):
    """
    Computes the PDF by normalizing the curve.
    Parameters:
        x (float): The input value.
        alpha (float): The location parameter.
        delta (float): The scale parameter.
        s (float): Shape parameter for sinu_curve.
        k (float): Exponent parameter for sinu_curve.
        flip (bool): Whether to flip the sinusoidal curve.
    Returns:
        float: The PDF value at x.
    """
    # Calculate the normalization constant
    area, _ = integrate.quad(lambda z: sinu_curve(z, s, k, flip), 0, 1)
    if area == 0:
        raise ValueError("The area under the curve is zero, cannot normalize the PDF.")
    
    # Normalize the curve
    if alpha <= x <= alpha + delta:
        z = (x - alpha) / delta
        return 1 / (area * delta) * curve_inner(z, s, k, flip)
    else:
        return 0

def cdf(x, alpha=0, delta=1, s=1, k=1, flip=False):
    """
    Computes the CDF based on the PDF.
    Parameters:
        x (float): The input value.
        alpha (float): The location parameter.
        delta (float): The scale parameter.
        s (float): Shape parameter for sinu_curve.
        k (float): Exponent parameter for sinu_curve.
        flip (bool): Whether to flip the sinusoidal curve.
    Returns:
        float: The CDF value at x.
    """
    if x < alpha:
        return 0
    elif x > alpha + delta:
        return 1
    else:
        result, _ = integrate.quad(lambda t: pdf(t, alpha, delta, s, k, flip), alpha, x)
        return result

def plot_cdf(alpha=0, delta=1, s=1, k=1, flip=False):
    """
    Plots the CDF over a specified range of x values.
    Parameters:
        alpha (float): The location parameter.
        delta (float): The scale parameter.
        s (float): Shape parameter for sinu_curve.
        k (float): Exponent parameter for sinu_curve.
        flip (bool): Whether to flip the sinusoidal curve.
    """
    # Define the range of x values for plotting
    x_values = np.linspace(alpha - 2 * delta, alpha + 2 * delta, 400)
    y_values = [cdf(x, alpha, delta, s, k, flip) for x in x_values]

    # Plot the result
    plt.plot(x_values, y_values, label='CDF', color='orange')
    plt.xlabel('x')
    plt.ylabel('CDF(x)')
    plt.title(f'Plot of the CDF (alpha={alpha}, delta={delta}, s={s}, k={k}, flip={flip})')
    plt.legend()
    plt.grid(True)
    plt.show()

def rmom(r, alpha=0, delta=1, s=1, k=1, flip=False):
    """
    Computes the r-th moment of the PDF.
    Parameters:
        r (int): The order of the moment.
        alpha (float): The location parameter.
        delta (float): The scale parameter.
        s (float): Shape parameter for sinu_curve.
        k (float): Exponent parameter for sinu_curve.
        flip (bool): Whether to flip the sinusoidal curve.
    Returns:
        float: The r-th moment of the PDF.
    """
    integrand = lambda x: x**r * pdf(x, alpha, delta, s, k, flip)
    moment, _ = integrate.quad(integrand, alpha, alpha + delta)
    return moment

def skewness(alpha=0, delta=1, s=1, k=1, flip=False):
    """
    Computes the skewness of the PDF.
    Parameters:
        alpha (float): The location parameter.
        delta (float): The scale parameter.
        s (float): Shape parameter for sinu_curve.
        k (float): Exponent parameter for sinu_curve.
        flip (bool): Whether to flip the sinusoidal curve.
    Returns:
        float: The skewness of the PDF.
    """
    mu = rmom(1, alpha, delta, s, k, flip)
    mu2 = rmom(2, alpha, delta, s, k, flip)
    mu3 = rmom(3, alpha, delta, s, k, flip)
    
    return (mu3 - 3 * mu * mu2 + 2 * mu**3) / ((mu2 - mu**2) ** 1.5)

def kurtosis(alpha=0, delta=1, s=1, k=1, flip=False):
    """
    Computes the kurtosis of the PDF.
    Parameters:
        alpha (float): The location parameter.
        delta (float): The scale parameter.
        s (float): Shape parameter for sinu_curve.
        k (float): Exponent parameter for sinu_curve.
        flip (bool): Whether to flip the sinusoidal curve.
    Returns:
        float: The kurtosis of the PDF.
    """
    mu2 = rmom(2, alpha, delta, s, k, flip)
    mu4 = rmom(4, alpha, delta, s, k, flip)
    
    return mu4 / (mu2 ** 2) - 3

# Example usage
plot_cdf(0.5, 1, s=1/3, k=1, flip=False)

# Compute and print the 1st, 2nd moments, skewness, and kurtosis for example parameters
print("1st moment (mean):", rmom(1, 0.5, 1, s=1/3, k=1, flip=False))
print("2nd moment:", rmom(2, 0.5, 1, s=1/3, k=1, flip=False))
print("Skewness:", skewness(0.5, 1, s=1/3, k=1, flip=False))
print("Kurtosis:", kurtosis(0.5, 1, s=1/3, k=1, flip=False))
