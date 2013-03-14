package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.order.Order;

public class IpRangeToSpanningCidr implements BinaryDelegate<Cidr, Ip, Ip> {

    @Override
    public Cidr perform(Ip firstIp, Ip lastIp) {
        dbc.precondition(firstIp != null, "startIp cannot be null");
        dbc.precondition(lastIp != null, "endIp cannot be null");
        dbc.precondition(Order.of(firstIp.compareTo(lastIp)) != Order.GT, "endIp cannot be lesser than startIp");
        Mask netmask = null;
        Cidr candidate;
        do {
            netmask = (netmask == null ? Mask.NARROWEST : netmask.widenHosts());
            candidate = Cidr.byContainedIp(lastIp, netmask);
        } while (!candidate.contains(firstIp));
        return candidate;
    }
}
