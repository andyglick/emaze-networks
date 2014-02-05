package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;

public class Ipv6PreviousIp implements Delegate<Ipv6, Ipv6> {

    @Override
    public Ipv6 perform(Ipv6 ip) {
        dbc.precondition(ip != null, "ip cannot be null");
        return ip.previous();
    }
}
