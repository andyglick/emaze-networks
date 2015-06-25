package net.emaze.networks.ipv4;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;

public class Ipv4PreviousIp implements Delegate<Ipv4, Ipv4> {

    @Override
    public Ipv4 perform(Ipv4 ip) {
        dbc.precondition(ip != null, "ip cannot be null");
        return ip.previous();
    }
}
