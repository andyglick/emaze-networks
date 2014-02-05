package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv6;
import org.junit.Test;

public class Ipv6FromStringTest {

    @Test
    public void deserializingIPv6YieldsExpected() throws IOException {
        final String serialized = "{'ip':'1234:0000:0000:0000:0000:0000:0000:0001'}".replace("'", "\"");
        final Ipv6 expected = Ipv6.parse("1234::1");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv6 got = mapper.readValue(serialized, BeanWithIpv6.class);
        Assert.assertEquals(expected, got.getIp());
    }
}
