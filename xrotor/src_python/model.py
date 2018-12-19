import numpy as np

from ctypes import c_bool, c_float, c_int
from typing import Dict


class Conditions(object):

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

    def __init__(self):
        super().__init__()
        self.r_hub = 0
        self.r_tip = 1
        self.r_wake = 0
        self.rake = 0
        self.geomdata: np.ndarray = np.zeros((4, 0), dtype=c_float, order='F')

    @property
    def n_geom(self):
        return self.geomdata.shape[1]

    @n_geom.setter
    def n_geom(self, n_geom: int):
        self.geomdata.resize((4, n_geom))

    @property
    def radii(self) -> np.ndarray:
        return self.geomdata[0, :]

    @radii.setter
    def radii(self, radii: np.ndarray):
        if radii.size != self.geomdata.shape[1]:
            self.geomdata.resize((4, radii.size))
        self.geomdata[0, :] = radii[:]

    @property
    def chord(self) -> np.ndarray:
        return self.geomdata[1, :]

    @chord.setter
    def chord(self, chord: np.ndarray):
        if chord.size != self.geomdata.shape[1]:
            self.geomdata.resize((4, chord.size))
        self.geomdata[1, :] = chord[:]

    @property
    def twist(self) -> np.ndarray:
        return self.geomdata[2, :]

    @twist.setter
    def twist(self, twist: np.ndarray):
        if twist.size != self.geomdata.shape[1]:
            self.geomdata.resize((4, twist.size))
        self.geomdata[2, :] = twist[:]

    @property
    def ubody(self) -> np.ndarray:
        return self.geomdata[3, :]

    @ubody.setter
    def ubody(self, ubody: np.ndarray):
        if ubody.size != self.geomdata.shape[1]:
            self.geomdata.resize((4, ubody.size))
        self.geomdata[3, :] = ubody[:]


class Section(object):

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
        sec = Section()
        for key, value in d.items():
            if hasattr(sec, key):
                setattr(sec, key, value)
        return sec


class Blade(object):

    def __init__(self, geometry: Geometry = Geometry(), sections: Dict[float, Section] = None):
        super().__init__()
        self.geometry: Geometry = geometry
        self.sections: Dict[float, sections] = dict() if sections is None else sections

    @property
    def n_aero(self) -> int:
        return len(self.sections)

    @property
    def aerodata(self) -> np.ndarray:
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
        return self.geometry.geomdata


class Disk(object):

    def __init__(self, n_blds: int = 0, blade: Blade = Blade()):
        super().__init__()
        self.n_blds: int = n_blds
        self.blade: Blade = blade


class Settings(object):

    def __init__(self, free=True, duct=False, wind=False):
        super().__init__()
        self.free = free
        self.duct = duct
        self.wind = wind


class Case(object):

    def __init__(self,
                 conditions: Conditions = Conditions(),
                 disk: Disk = Disk(), settings: Settings = Settings()):
        super().__init__()
        self.conditions: Conditions = conditions
        self.disk: Disk = disk
        self.settings: Settings = settings

    @staticmethod
    def from_dict(d):
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

    def __init__(self, rpm: float = 0, thrust: float = 0, torque: float = 0, power: float = 0, efficiency: float = 0):
        super().__init__()
        self._rpm: c_float = c_float(rpm)
        self._thrust: c_float = c_float(thrust)
        self._torque: c_float = c_float(torque)
        self._power: c_float = c_float(power)
        self._efficiency: c_float = c_float(efficiency)

    @property
    def rpm(self):
        return self._rpm.value

    @property
    def thrust(self):
        return self._thrust.value

    @property
    def torque(self):
        return self._torque.value

    @property
    def power(self):
        return self._power.value

    @property
    def efficiency(self):
        return self._efficiency.value

