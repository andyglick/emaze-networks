package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;

public class PreviousIpv4 implements Delegate<Ip, Ip> {

    @Override
    public Ip perform(Ip ip) {
        dbc.precondition(ip != null, "ip cannot be null");
        return ip.previous();
    }
}
