package net.emaze.networks;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.Multiplexing;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.order.Order;

public class Ipv6RangeToNetworks implements BinaryDelegate<List<Ipv6Network>, Ipv6, Ipv6> {

    private final Ipv6SortNetworksByFirstThenLastIp sorter = new Ipv6SortNetworksByFirstThenLastIp();

    @Override
    public List<Ipv6Network> perform(Ipv6 firstIp, Ipv6 lastIp) {
        dbc.precondition(firstIp != null, "firstIp cannot be null");
        dbc.precondition(lastIp != null, "lastIp cannot be null");
        dbc.precondition(Order.of(firstIp.compareTo(lastIp)) != Order.GT, "lastIp cannot be lesser than firstIp");
        // Obtain a spanning cidr containing entire range
        final Ipv6Network spanningCidr = new Ipv6RangeToSpanningNetwork().perform(firstIp, lastIp);
        if (spanningCidr.firstIp().equals(firstIp) && spanningCidr.lastIp().equals(lastIp)) {
            // Spanning CIDR matches start and end exactly;
            return Collections.singletonList(spanningCidr);
        }
        if (spanningCidr.lastIp().equals(lastIp)) {
            // Spanning CIDR matches range end exactly;
            return pruneSpanningCidrHead(spanningCidr, firstIp);
        }
        if (spanningCidr.firstIp().equals(firstIp)) {
            // Spanning CIDR matches range start exactly
            return pruneSpanningCidrTail(spanningCidr, lastIp);
        }
        // Spanning CIDR overlaps entire range
        final LinkedList<Ipv6Network> prunedHead = pruneSpanningCidrHead(spanningCidr, firstIp);
        final List<Ipv6Network> prunedTail = pruneSpanningCidrTail(prunedHead.removeLast(), lastIp);
        return Consumers.all(Multiplexing.flatten(prunedHead, prunedTail));
    }

    private LinkedList<Ipv6Network> pruneSpanningCidrHead(Ipv6Network head, Ipv6 firstIp) {
        final Ipv6 previousIp = firstIp.previous();
        final LinkedList<Ipv6Network> result = new LinkedList<>();
        final List<Ipv6Network> remainder = sorter.perform(new Ipv6SubtractIpFromNetwork().perform(head, previousIp));
        boolean firstFound = false;
        for (Ipv6Network cidr : remainder) {
            if (cidr.firstIp().equals(firstIp)) {
                firstFound = true;
            }
            if (firstFound) {
                result.add(cidr);
            }
        }
        return result;
    }

    private LinkedList<Ipv6Network> pruneSpanningCidrTail(Ipv6Network tail, Ipv6 lastIp) {
        final Ipv6 nextIp = lastIp.next();
        final LinkedList<Ipv6Network> result = new LinkedList<>();
        final List<Ipv6Network> remainder = sorter.perform(new Ipv6SubtractIpFromNetwork().perform(tail, nextIp));
        for (Ipv6Network cidr : remainder) {
            result.add(cidr);
            if (cidr.lastIp().equals(lastIp)) {
                break;
            }
        }
        return result;
    }
}
