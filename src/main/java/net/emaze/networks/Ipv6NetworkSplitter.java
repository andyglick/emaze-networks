package net.emaze.networks;

import java.util.LinkedList;
import java.util.List;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;

public class Ipv6NetworkSplitter implements BinaryDelegate<List<Ipv6Network>, Ipv6Network, Integer> {

    @Override
    public List<Ipv6Network> perform(Ipv6Network source, Integer newNetmaskSize) {
        dbc.precondition(source != null, "Cannot split a null network");
        dbc.precondition(newNetmaskSize != null, "Cannot split to a null netmask");
        final List<Ipv6Network> networks = new LinkedList<>();
        final Ipv6Mask mask = Ipv6Mask.net(newNetmaskSize);
        final FixedSizeNatural base = source.firstIp().bits();
        final int bits = newNetmaskSize - source.netmask().population();
        final FixedSizeNatural last = FixedSizeNatural.one(128).shiftLeft(bits);
        for (FixedSizeNatural delta = FixedSizeNatural.zero(128); !delta.equals(last); delta = delta.increment()) {
            final FixedSizeNatural address = base.or(delta.shiftLeft(128 - newNetmaskSize));
            networks.add(Ipv6Network.fromCidrNotation(new Ipv6(address), mask));
        }
        return networks;
    }

}
