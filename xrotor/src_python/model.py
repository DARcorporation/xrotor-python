import numpy as np

from ctypes import c_float
from typing import Dict


class Conditions(object):
    """Propeller operating conditions

    Attributes
    ----------
    rho : float
        Air density in kg/m^3
    vso : float
        Speed of sound in m/s
    rmu : float
        Dynamic viscosity in
    vel : float
        Inflow velocity in m/s
    adv : float
        Advance ratio
        It is defined as J = V/(ωR), where J is the advance ratio, V is the inflow velocity, ω is the rotational rate in
        rad/s, and R is the blade tip radius in m.
    """

    def __init__(self,
                 rho= 0, vso=0, rmu=0,
                 alt=0, vel=0, adv=0):
        super().__init__()
        self.rho = rho
        self.vso = vso
        self.rmu = rmu
        self.alt = alt
        self.vel = vel
        self.adv = adv


class Geometry(object):
    """Propeller blade geometry definition

    Attributes
    ----------
    r_hub : float
        Hub radius in m
    r_tip : float
        Blade tip radius in m
    r_wake : float
        Wake blockage radius in m
    rake : float
        Rake angle
    geomdata : np.ndarray
        Array with the geometric data
    n_geom
    radii
    chord
    twist
    ubody
    """

    def __init__(self):
        super().__init__()
        self.r_hub = 0
        self.r_tip = 1
        self.r_wake = 0
        self.rake = 0
        self.geomdata = np.zeros((4, 0), dtype=c_float)

    @property
    def n_geom(self):
        """int: Number of points at which the geometric data is recorded."""
        return self.geomdata.shape[1]

    @n_geom.setter
    def n_geom(self, n_geom: int):
        self.geomdata.resize((4, n_geom))

    @property
    def radii(self) -> np.ndarray:
        """np.ndarray: List of normalized radial station locations."""
        return self.geomdata[0, :]

    @radii.setter
    def radii(self, radii: np.ndarray):
        if radii.size != self.geomdata.shape[1]:
            self.geomdata.resize((4, radii.size))
        self.geomdata[0, :] = radii[:]

    @property
    def chord(self) -> np.ndarray:
        """np.ndarray: List of normalized chord lengths for each radial station."""
        return self.geomdata[1, :]

    @chord.setter
    def chord(self, chord: np.ndarray):
        if chord.size != self.geomdata.shape[1]:
            self.geomdata.resize((4, chord.size))
        self.geomdata[1, :] = chord[:]

    @property
    def twist(self) -> np.ndarray:
        """np.ndarray: List of twist angles for each radial station in degrees."""
        return self.geomdata[2, :]

    @twist.setter
    def twist(self, twist: np.ndarray):
        if twist.size != self.geomdata.shape[1]:
            self.geomdata.resize((4, twist.size))
        self.geomdata[2, :] = twist[:]

    @property
    def ubody(self) -> np.ndarray:
        """np.ndarray: Nacelle perturbation axial velocities at each radial station in m/s."""
        return self.geomdata[3, :]

    @ubody.setter
    def ubody(self, ubody: np.ndarray):
        if ubody.size != self.geomdata.shape[1]:
            self.geomdata.resize((4, ubody.size))
        self.geomdata[3, :] = ubody[:]


class Section(object):
    """Aerodynamic section definition

    Attributes
    ----------
    a0 : float
        Angle of attack at zero lift in deg
    cl_max : float
        Maximum lift coefficient
    cl_min : float
        Minimum lift coefficient
    dcl_da : float
        Lift curve slope, d(c_l)/d(α)
    dcl_da_stall : float
        Lift curve slope in post-stall regime, [d(C_l)/d(α)]_stall
    dcl_stall : float
        Lift increment from the onset of stall till full stall occurs, (δC_l)_stall
    cd_min : float
        Minimum drag coefficient
    cl_cd_min : float
        Lift coefficient at minimum drag coefficient
    dcd_dcl2 : float
        Parabolic drag parameter, d(C_l)/d(C_d^2)
    m_crit : float
        Critical Mach number
    cm_const : float
        Moment coefficient in the linear range
    re_ref : float
        Reference Reynolds number
    re_exp : float
        Reynolds number exponent, such that C_d ~ Re^re_exp
    """

    def __init__(self,
                 a0=0, cl_max=0, cl_min=0,
                 dcl_da=0, dcl_da_stall=0, dcl_stall=0.,
                 cd_min=0, cl_cd_min=0, dcd_dcl2=0,
                 cm_const=0, m_crit=.6, re_ref=0, re_exp=-.4):
        super().__init__()
        self.a0 = a0
        self.cl_max = cl_max
        self.cl_min = cl_min
        self.dcl_da = dcl_da
        self.dcl_da_stall = dcl_da_stall
        self.dcl_stall = dcl_stall
        self.cd_min = cd_min
        self.cl_cd_min = cl_cd_min
        self.dcd_dcl2 = dcd_dcl2
        self.cm_const = cm_const
        self.m_crit = m_crit
        self.re_ref = re_ref
        self.re_exp = re_exp

    @staticmethod
    def from_dict(d):
        """Create an instance of the Section class from a dictionary.

        Parameters
        ----------
        d : dict
            Dictionary containing relevant aerodynamic properties

        Returns
        -------
        Section
            Instance of this class with the data provided through the dictionary
        """
        sec = Section()
        for key, value in d.items():
            if hasattr(sec, key):
                setattr(sec, key, value)
        return sec


class Blade(object):
    """Propeller blade definition

    Attributes
    ----------
    geometry : Geometry
        Instance of the Geometry class representing the geometry of the blade
    sections : dict of Section instances by float
        Dictionary of aerodynamic Sections, indexed by their normalized radial positions along the blade
    n_aero
    aerodata
    geomdata
    """

    def __init__(self, geometry: Geometry = Geometry(), sections: Dict[float, Section] = None):
        super().__init__()
        self.geometry: Geometry = geometry
        self.sections: Dict[float, sections] = dict() if sections is None else sections

    @property
    def n_aero(self) -> int:
        """int: Number of aerodynamic Sections"""
        return len(self.sections)

    @property
    def aerodata(self) -> np.ndarray:
        """np.ndarray: Array with all Sections' data, row-wise, indexed by radial position in the first column"""
        data = np.zeros((14, self.n_aero), dtype=c_float, order='F')
        for i, key in enumerate(sorted(self.sections.keys())):
            sec = self.sections[key]
            data[0][i] = key
            data[1][i] = sec.a0
            data[2][i] = sec.cl_max
            data[3][i] = sec.cl_min
            data[4][i] = sec.dcl_da
            data[5][i] = sec.dcl_da_stall
            data[6][i] = sec.dcl_stall
            data[7][i] = sec.cd_min
            data[8][i] = sec.cl_cd_min
            data[9][i] = sec.dcd_dcl2
            data[10][i] = sec.cm_const
            data[11][i] = sec.m_crit
            data[12][i] = sec.re_ref
            data[13][i] = sec.re_exp
        return data

    @property
    def geomdata(self) -> np.ndarray:
        """np.ndarray: Array of the blade's geometric data"""
        return self.geometry.geomdata


class Disk(object):
    """Propeller disk definition

    Attributes
    ----------
    n_blds : int
        Number of blades
    blade : Blade
        Definition of the blade
    """

    def __init__(self, n_blds: int = 0, blade: Blade = Blade()):
        super().__init__()
        self.n_blds: int = n_blds
        self.blade: Blade = blade


class Settings(object):
    """XRotor run case settings

    Attributes
    ----------
    free : bool
        True for free wake formulation
    duct : bool
        True if a duct is present
    wind : bool
        True if windmill-mode plotting is to be used
    """

    def __init__(self, free=True, duct=False, wind=False):
        super().__init__()
        self.free = free
        self.duct = duct
        self.wind = wind


class Case(object):
    """XRotor run case definition

    Attributes
    ----------
    conditions : Conditions
        Specification of the operating conditions
    disk : Disk
        Definition of the propeller disk
    settings : Settings
        Specification of the run case settings
    """

    def __init__(self,
                 conditions: Conditions = Conditions(),
                 disk: Disk = Disk(), settings: Settings = Settings()):
        super().__init__()
        self.conditions: Conditions = conditions
        self.disk: Disk = disk
        self.settings: Settings = settings

    @staticmethod
    def from_dict(d):
        """Construct an instance of this class from a dictionary.

        Parameters
        ----------
        d : dict
            Dictionary representing an XRotor run case

        Returns
        -------
        Case
            Instance of this class corresponding to the data passed through the dictionary
        """
        case = Case()

        def recurse(obj, sub_d):
            if isinstance(obj, dict):
                for key, value in sub_d.items():
                    obj.update({key: Section.from_dict(value)})
            else:
                for key, value in sub_d.items():
                    if hasattr(obj, key):
                        if isinstance(value, dict):
                            recurse(getattr(obj, key), value)
                        else:
                            setattr(obj, key, value)
        recurse(case, d)
        return case


class Performance(object):
    """Blade performance specification

    Attributes
    ----------
    rpm
    thrust
    torque
    power
    efficiency
    """

    def __init__(self, rpm: float = 0, thrust: float = 0, torque: float = 0, power: float = 0, efficiency: float = 0):
        super().__init__()
        self._rpm: c_float = c_float(rpm)
        self._thrust: c_float = c_float(thrust)
        self._torque: c_float = c_float(torque)
        self._power: c_float = c_float(power)
        self._efficiency: c_float = c_float(efficiency)

    @property
    def rpm(self):
        """float: Rotational speed in rev/min"""
        return self._rpm.value

    @property
    def thrust(self):
        """float: Thrust in N"""
        return self._thrust.value

    @property
    def torque(self):
        """float: Torque in Nm"""
        return self._torque.value

    @property
    def power(self):
        """float: Power in W"""
        return self._power.value

    @property
    def efficiency(self):
        """float: Overall efficiency"""
        return self._efficiency.value

