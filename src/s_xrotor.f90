module s_xrotor
contains
    subroutine uvadd(ctxt, xiw, wa, wt)
        use i_common, only : Common
        use m_spline, only : seval

        implicit real (m)
        type(Common), intent(inout) :: ctxt
        !
        wa = 0.0
        wt = 0.0
        !
        if(ctxt%nadd <= 1) return
        !
        rdim = xiw * ctxt%rad
        if(rdim >= ctxt%radd(1) .and. rdim <= ctxt%radd(ctxt%nadd)) then
            wa = seval(rdim, ctxt%uadd, ctxt%uaddr, ctxt%radd) / ctxt%vel
            wt = seval(rdim, ctxt%vadd, ctxt%vaddr, ctxt%radd) / ctxt%vel
        endif
        !
        return
    end
end module s_xrotor