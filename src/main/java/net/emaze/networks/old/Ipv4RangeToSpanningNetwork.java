package net.emaze.networks.old;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.order.Order;

public class Ipv4RangeToSpanningNetwork implements BinaryDelegate<Ipv4Network, Ipv4, Ipv4> {

    @Override
    public Ipv4Network perform(Ipv4 firstIp, Ipv4 lastIp) {
        dbc.precondition(firstIp != null, "startIp cannot be null");
        dbc.precondition(lastIp != null, "endIp cannot be null");
        dbc.precondition(Order.of(firstIp.compareTo(lastIp)) != Order.GT, "endIp cannot be lesser than startIp");
        Ipv4Mask netmask = null;
        Ipv4Network candidate;
        do {
            netmask = (netmask == null ? Ipv4Mask.NARROWEST : netmask.widenHosts());
            candidate = Ipv4Network.byContainedIp(lastIp, netmask);
        } while (!candidate.contains(firstIp));
        return candidate;
    }
}
