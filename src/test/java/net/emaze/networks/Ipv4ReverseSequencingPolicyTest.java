package net.emaze.networks;

import net.emaze.dysfunctional.options.Maybe;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4ReverseSequencingPolicyTest {

    @Test
    public void nextYieldsPreviousIp() {
        final Maybe<Ipv4> got = new Ipv4ReverseSequencingPolicy().next(Ipv4.parse("10.0.0.1"));
        Assert.assertEquals(Ipv4.parse("10.0.0.0"), got.value());
    }

    @Test
    public void nextYieldsNothingAppliedToFirstIp() {
        final Maybe<Ipv4> got = new Ipv4ReverseSequencingPolicy().next(Ipv4.FIRST_IP);
        Assert.assertFalse(got.hasValue());
    }
}