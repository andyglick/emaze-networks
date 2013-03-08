package net.emaze.networks;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.Multiplexing;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.order.Order;

public class IpRangeToCidrs implements BinaryDelegate<List<Cidr>, Ipv4, Ipv4> {

    @Override
    public List<Cidr> perform(Ipv4 firstIp, Ipv4 lastIp) {
        dbc.precondition(firstIp != null, "firstIp cannot be null");
        dbc.precondition(lastIp != null, "lastIp cannot be null");
        dbc.precondition(Order.of(firstIp.compareTo(lastIp)) != Order.GT, "lastIp cannot be lesser than firstIp");
        // Obtain a spanning cidr containing entire range
        final Cidr spanningCidr = new IpRangeToSpanningCidr().perform(firstIp, lastIp);
        if (spanningCidr.network().equals(firstIp) && spanningCidr.broadcast().equals(lastIp)) {
            // Spanning CIDR matches start and end exactly;
            return Collections.singletonList(spanningCidr);
        }
        if (spanningCidr.broadcast().equals(lastIp)) {
            // Spanning CIDR matches range end exactly;
            return pruneSpanningCidrHead(spanningCidr, firstIp);
        }
        if (spanningCidr.network().equals(firstIp)) {
            // Spanning CIDR matches range start exactly
            return pruneSpanningCidrTail(spanningCidr, lastIp);
        }
        // Spanning CIDR overlaps entire range
        final LinkedList<Cidr> prunedHead = pruneSpanningCidrHead(spanningCidr, firstIp);
        final List<Cidr> prunedTail = pruneSpanningCidrTail(prunedHead.removeLast(), lastIp);
        return Consumers.all(Multiplexing.flatten(prunedHead, prunedTail));
    }

    private LinkedList<Cidr> pruneSpanningCidrHead(Cidr head, Ipv4 firstIp) {
        final Ipv4 previousIp = firstIp.offset(-1);
        final LinkedList<Cidr> result = new LinkedList<>();
        final List<Cidr> remainder = new SubtractIpFromCidr().perform(head, previousIp);
        boolean firstFound = false;
        for (Cidr cidr : remainder) {
            if (cidr.network().equals(firstIp)) {
                firstFound = true;
            }
            if (firstFound) {
                result.add(cidr);
            }
        }
        return result;
    }

    private LinkedList<Cidr> pruneSpanningCidrTail(Cidr tail, Ipv4 lastIp) {
        final Ipv4 nextIp = lastIp.offset(1);
        final LinkedList<Cidr> result = new LinkedList<>();
        final List<Cidr> remainder = new SubtractIpFromCidr().perform(tail, nextIp);
        for (Cidr cidr : remainder) {
            result.add(cidr);
            if (cidr.broadcast().equals(lastIp)) {
                break;
            }
        }
        return result;
    }
}
