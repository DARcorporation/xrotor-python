!*==S_XROTOR.f90  processed by SPAG 7.25DB at 09:24 on  2 Aug 2019
module s_xrotor
    implicit none
contains
    subroutine uvadd(ctxt, xiw, wa, wt)

        use i_common, only : Common
        use m_spline, only : seval
        implicit real(M)
        !*** Start of declarations inserted by SPAG
        real RDIM, WA, WT, XIW
        !*** End of declarations inserted by SPAG
        type (Common), intent(inout) :: ctxt
        !
        wa = 0.0
        wt = 0.0
        !
        if (ctxt%nadd<=1) return
        !
        rdim = xiw * ctxt%rad
        if (rdim>=ctxt%radd(1).and.rdim<=ctxt%radd(ctxt%nadd)) then
            wa = seval(rdim, ctxt%uadd, ctxt%uaddr, ctxt%radd) / ctxt%vel
            wt = seval(rdim, ctxt%vadd, ctxt%vaddr, ctxt%radd) / ctxt%vel
        endif
        !
    end
end
