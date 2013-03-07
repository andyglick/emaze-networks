package net.emaze.networks;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.Multiplexing;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.ranges.DenseRange;

public class IpRangeToCidrs implements Delegate<List<Cidr>, DenseRange<Ipv4>> {

    @Override
    public List<Cidr> perform(DenseRange<Ipv4> range) {
        dbc.precondition(range != null, "range cannot be null");
        dbc.precondition(range.end().hasValue(), "range cannot be open-ended");
        if (!range.iterator().hasNext()) {
            // An empty range results in an empty list of CIDRs
            return Collections.emptyList();
        }
        // Obtain a spanning cidr containing entire range
        final Cidr spanningCidr = new IpRangeToSpanningCidr().perform(range);
        final Ipv4 startIp = range.begin();
        // FIXME FIXME FIXME: A closed range SHOULD NOT return as last ip the first ip outside range !!!!!!!!!!
        final Ipv4 endIp = range.end().value().previous();
        if (spanningCidr.first().equals(startIp) && spanningCidr.last().equals(endIp)) {
            // Spanning CIDR matches start and end exactly;
            return Collections.singletonList(spanningCidr);
        }
        if (spanningCidr.last().equals(endIp)) {
            // Spanning CIDR matches range end exactly;
            return pruneSpanningCidrHead(spanningCidr, startIp);
        }
        if (spanningCidr.first().equals(startIp)) {
            // Spanning CIDR matches range start exactly
            return pruneSpanningCidrTail(spanningCidr, endIp);
        }
        // Spanning CIDR overlaps entire range
        final LinkedList<Cidr> prunedHead = pruneSpanningCidrHead(spanningCidr, startIp);
        final List<Cidr> prunedTail = pruneSpanningCidrTail(prunedHead.removeLast(), endIp);
        return Consumers.all(Multiplexing.flatten(prunedHead, prunedTail));
    }

    private LinkedList<Cidr> pruneSpanningCidrHead(Cidr head, Ipv4 startIp) {
        final Ipv4 previousIp = startIp.previous();
        final LinkedList<Cidr> result = new LinkedList<>();
        final List<Cidr> remainder = new SubtractIpFromCidr().perform(head, previousIp);
        boolean firstFound = false;
        for (Cidr cidr : remainder) {
            if (cidr.first().equals(startIp)) {
                firstFound = true;
            }
            if (firstFound) {
                result.add(cidr);
            }
        }
        return result;
    }

    private LinkedList<Cidr> pruneSpanningCidrTail(Cidr tail, Ipv4 endIp) {
        final Ipv4 nextIp = endIp.next();
        final LinkedList<Cidr> result = new LinkedList<>();
        final List<Cidr> remainder = new SubtractIpFromCidr().perform(tail, nextIp);
        for (Cidr cidr : remainder) {
            result.add(cidr);
            if (cidr.last().equals(endIp)) {
                break;
            }
        }
        return result;
    }
}
