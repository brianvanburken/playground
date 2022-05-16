def least_observed(particles: dict) -> str:
    """ Returns the particle least likely observed.

    >>> least_observed({
    ...     'neotron': 0.55,
    ...     'proton': 0.21,
    ...     'meson': 0.03,
    ...     'muon': 0.07,
    ...     'neutrino': 0.14
    ... })
    'meson'
    """
    least_probability = min(particles.values())
    for particle, probability in particles.items():
        if probability == least_probability:
            return particle
    return None
