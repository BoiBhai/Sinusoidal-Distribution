# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
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

# Example usage
plot_cdf(0.5, 1, s=1/3, k=1, flip=False)
